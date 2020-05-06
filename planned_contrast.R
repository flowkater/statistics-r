# 계획된 대비 Planned Contrast 

contrast <- read.csv("./data/Contrast.csv", header = T)
contrast

contrast.lm <- lm(libido ~ dummy1 + dummy2, data = contrast)
summary(contrast.lm)

contrasts(melted_marketing$channel)

contrast1 <- c(1, 1, -1, -1, -1, 1)
contrast2 <- c(-1, -1, 0, 0, 0, 2)


contrasts(melted_marketing$channel) <- cbind(contrast1, contrast2, 0, 0, 0)
contrasts(melted_marketing$channel)

marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary.lm(marketing.aov)


contrastBanner <- c(1, 1, -5, 1, 1, 1)
contrastBlog <- c(1, 1, 0, 1, -4, 1)
contrastEmail <- c(1, 1, 0, -3, 0, 1)
contrastSMS <- c(-2, 1, 0, 0, 0, 1)
contrastPush <- c(0, -1, 0, 0, 0, 1)

contrasts(melted_marketing$channel) <- cbind(contrastBanner, contrastBlog, contrastEmail, contrastSMS, contrastPush)
contrasts(melted_marketing$channel)


marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary.lm(marketing.aov)

# 대비(contrast) 생성
contrasts(melted_marketing$channel) <- contr.helmert(6);contrasts(melted_marketing$channel)

marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary.lm(marketing.aov)

# 추세 분석
contrasts(melted_marketing$channel) <- contr.poly(6);contrasts(melted_marketing$channel)

marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary.lm(marketing.aov)

marketing.trend <- ggplot(melted_marketing, aes(channel, count)) + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), colour = "Blue", linetype = "dashed") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Intervention", y = "Mean Number of Hiccups")
marketing.trend

marketing <- read.csv("./data/Marketing Revenue.csv", header = T)
melted_marketing <- melt(marketing)
names(melted_marketing) <- c("channel", "count")

marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary.lm(marketing.aov)


# r 대비
rcontrast <- function(t, df){
  return(sqrt(t^2 / (t^2 + df)))
}

contrast1 <- c(1, 1, -1, -1, -1, 1)
contrast2 <- c(-1, -1, 0, 0, 0, 2)

contrasts(melted_marketing$channel) <- cbind(contrast1, contrast2, 0, 0, 0)
contrasts(melted_marketing$channel)

marketing.aov <- aov(count ~ channel, data = melted_marketing)
summary(marketing.aov)
aovlm <- summary.lm(marketing.aov)

t1 <- aovlm$coefficients[[2,3]];t1
t2 <- aovlm$coefficients[[3,3]];t2

N <- 72
p <- 2
df <- N - p - 1;df

rcontrast(t1, df)
rcontrast(t2, df)
