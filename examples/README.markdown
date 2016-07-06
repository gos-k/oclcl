# Installation

```
git clone https://github.com/cffi/cffi.git ~/.roswell/local-projects/cffi
git clone https://github.com/guicho271828/eazy-opencl ~/.roswell/local-projects/eazy-opencl
ros install cffi
ros install cffi-grovel
ros install eazy-opencl
```

# Execution

```
ros run
```

```
(ql:quickload :oclcl)
(ql:quickload :oclcl-examples)
(oclcl-examples.vector-add:main)
(oclcl-examples.diffuse0:main)
(oclcl-examples.diffuse1:main)
(oclcl-examples.vector-add-oclapi:main)
(oclcl-examples.diffuse0-oclapi:main)
(oclcl-examples.diffuse1-oclapi:main)
(oclcl-examples.sph-oclapi:main)
```

# cl-cuda to oclcl

thread-idx-x -> (get-local-id 0)
block-idx-x -> (get-group-id 0)
block-idx-y -> (get-group-id 1)
(+ (* block-idx-x block-dim-x) thread-idx-x) -> (get-global-id 0)
