# R-package: ssmodels
## Time series using state space methods

The motivation behind this R package was to develop a suite of tools using state space methods from first principles. Hence, allowing for complete control over the underlying methodologies. This package has no required dependencies, and makes no direct calls to underlying C functions. This is useful for educational purposes, research, or for re-factoring to another language. 

The following methods are supported:
- ARIMA
- ARIMAX
- Local Level
- Local Level Trend
- Basic Structural Model

### Build & Install

```
> cd /path/to/repos/ssmodels
> R CMD build src
> R CMD INSTALL ssmodels_0.1-0.tar.gz
```

### Authors

* **Ian Moore** - [icmoore GH](https://github.com/icmoore)

### License

This project is licensed under the MIT License 

### References

J. Durbin and S. J. Koopman. [Time Series Analysis by State Space Methods: Second Edition](https://www.oxfordscholarship.com/view/10.1093/acprof:oso/9780199641178.001.0001/acprof-9780199641178). Oxford University Press, 2012.
