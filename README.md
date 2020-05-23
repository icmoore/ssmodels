# R-package: ssmodels
## Time series using state space methods

The motivation behind this R package was to create a suite of tools using state space methods from first principles. Hence, having full control of the underlying code base. This package has no dependencies, and will install and run on the base R system. This is useful for educational purposes, research, or as a base point for re-factoring to another language. 

The following methods are supported:
- ARIMA
- ARIMAX
- Local Level
- Local Level Trend
- Basic Structural Model

### Installing

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
