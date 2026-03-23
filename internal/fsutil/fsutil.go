package fsutil

import (
	"io"
	"io/fs"
	"os"
	"path/filepath"
)

func CopyDir(src, dst string) error {
	return filepath.WalkDir(src, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		target := filepath.Join(dst, path[len(src):])

		if d.IsDir() {
			return os.MkdirAll(target, 0755)
		}

		return CopyFile(path, target)
	})
}

func CopyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, in)
	return err
}
