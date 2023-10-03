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