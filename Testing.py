#!/usr/bin/env python3
import argparse, shutil, sys, os
from pathlib import Path
from typing import List, Literal
from itertools import cycle

Mode = Literal["copy", "hardlink", "symlink"]

def iter_files(src_dir: Path, recursive: bool) -> List[Path]:
    if recursive:
        return [p for p in src_dir.rglob("*") if p.is_file()]
    else:
        return [p for p in src_dir.iterdir() if p.is_file()]

def unique_target(base: Path) -> Path:
    """
    If `base` exists, append __1, __2, ... before the extension until a free name is found.
    """
    if not base.exists():
        return base
    stem, suffix = base.stem, base.suffix
    parent = base.parent
    k = 1
    while True:
        candidate = parent / f"{stem}__{k}{suffix}"
        if not candidate.exists():
            return candidate
        k += 1

def place_one(src_file: Path, dst_dir: Path, index: int, width: int, mode: Mode, prefix: str | None) -> Path:
    stem, suffix = src_file.stem, src_file.suffix
    # If prefix is provided, prepend it to the stem; otherwise keep original stem
    name_stem = f"{prefix}{stem}" if prefix else stem
    target = dst_dir / f"{name_stem}_copy_{index:0{width}d}{suffix}"
    target = unique_target(target)

    if mode == "copy":
        shutil.copy2(src_file, target)
    elif mode == "hardlink":
        os.link(src_file, target)
    elif mode == "symlink":
        os.symlink(src_file, target)
    else:
        raise ValueError("mode must be one of: copy | hardlink | symlink")
    return target

def main():
    p = argparse.ArgumentParser(
        description="Make a flat set of file copies in DEST, totaling exactly --count."
    )
    p.add_argument("--src", required=True, help="Path to source folder containing files")
    p.add_argument("--dst", required=True, help="Destination folder to put copies (flat)")
    p.add_argument("--count", type=int, required=True, help="Total number of copies to create")
    p.add_argument("--mode", choices=["copy","hardlink","symlink"], default="copy",
                   help="copy=real bytes, hardlink=saves space (same filesystem), symlink=pointer")
    p.add_argument("--prefix", default=None,
                   help="Optional prefix to prepend to each output filename's stem")
    p.add_argument("--start", type=int, default=1, help="Starting index (default 1)")
    p.add_argument("--recursive", action="store_true",
                   help="Include files from subfolders (flattened into DEST)")
    p.add_argument("--dry-run", action="store_true",
                   help="Show what would be created without writing anything")
    args = p.parse_args()

    src_dir = Path(args.src).resolve()
    dst_dir = Path(args.dst).resolve()

    if not src_dir.is_dir():
        sys.exit(f"Source does not exist or is not a directory: {src_dir}")
    if args.count <= 0:
        sys.exit("--count must be a positive integer")

    files = iter_files(src_dir, recursive=args.recursive)
    if not files:
        sys.exit(f"No files found in {src_dir} (recursive={args.recursive}).")

    dst_dir.mkdir(parents=True, exist_ok=True)

    width = max(4, len(str(args.start + args.count - 1)))
    file_cycle = cycle(files)

    created = 0
    for i in range(args.start, args.start + args.count):
        src_file = next(file_cycle)
        stem, suffix = src_file.stem, src_file.suffix
        name_stem = f"{args.prefix}{stem}" if args.prefix else stem
        target = dst_dir / f"{name_stem}_copy_{i:0{width}d}{suffix}"
        target = unique_target(target)

        if args.dry_run:
            # Print the first few and last one to avoid huge output
            if created < 20 or i == (args.start + args.count - 1):
                print(f"[dry-run] {src_file.name} -> {target.name}")
            created += 1
            continue

        try:
            if args.mode == "copy":
                shutil.copy2(src_file, target)
            elif args.mode == "hardlink":
                os.link(src_file, target)
            elif args.mode == "symlink":
                os.symlink(src_file, target)
            created += 1
        except Exception as e:
            print(f"Failed: {src_file} -> {target} ({e})")

    if args.dry_run:
        print(f"[dry-run] Would create exactly {created} files in: {dst_dir}")
    else:
        print(f"Done. Created exactly {created} files in: {dst_dir}")

if __name__ == "__main__":
    main()