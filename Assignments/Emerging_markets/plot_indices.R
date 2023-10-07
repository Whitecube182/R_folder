#Testing plotting of the data from normalized all together

ggplot(
  data = normalized,
  mapping = aes(x = Name)
) +
  geom_line(aes(y = FTSEWorldN, color = "red")) +
  geom_line(aes(y = IndiaN, color = "purple")) +
  geom_line(aes(y = EgyptN, color = "darkblue")) +
  geom_line(aes(y = ChinaN, color = "blue")) +
  geom_line(aes(y = BrazilN, color = "orange")) +
  geom_line(aes(y = TaiwanN, color = "pink"))

#Making 6 different graphs presented beside eachother

ggplot(normalized, aes(x = ))

ggplot(
  data = normalized_trimmed,
  mapping = aes(x = Date)
) +
  geom_line(aes(y = FTSE_per, color = "red")) +
  geom_line(aes(y = China_per, color = "green")) +
  geom_line(aes(y = Brazil_per, color = "gray")) +
  geom_line(aes(y = Taiwan_per, color = "pink")) +
  geom_line(aes(y = Brazil_per, color = "blue")) +
  geom_line(aes(y = Taiwan_per, color = "orange"))
