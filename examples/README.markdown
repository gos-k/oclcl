# Installation

```
git clone https://github.com/cffi/cffi.git ~/.roswell/local-projects/cffi
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
```