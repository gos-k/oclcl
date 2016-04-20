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
```
