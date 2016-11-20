test <- data.frame(x=c(1, 2, 3, 4, 5), y=c(2, 2, 1.75, 2.25, 2))
model <- lm(data = test, y~x)
report <- summary(model)
report$r.squared
model
plot(test)
plot(model)

# Model 1
model_mtcars <- lm(data = mtcars, mpg~
                     disp+hp+wt+am)
report_mtcars <- summary(model_mtcars)
report_mtcars$r.squared

# Model 2
model_mtcars2 <- lm(data = mtcars, mpg~
                     cyl+hp+wt+am)
report_mtcars2 <- summary(model_mtcars2)
report_mtcars2$r.squared

# Model 3
model_mtcars3 <- lm(data = mtcars, mpg~
                      disp+cyl+wt+am)
report_mtcars3 <- summary(model_mtcars3)
report_mtcars3$r.squared

# Model 4
model_mtcars4 <- lm(data = mtcars, mpg~
                      disp+hp+cyl+am)
report_mtcars4 <- summary(model_mtcars4)
report_mtcars4$r.squared

report_mtcars
report_mtcars2
report_mtcars3
report_mtcars4