# Варіант 13 — Диспетчер прокату електросамокатів (`scooter_hub`)

Імпорт журналу подій з CSV через залізнично орієнтоване програмування (Either, без винятків
для бізнес-валідації), згортання подій у стан хабу, розрахунки та перевірка інваріантів.

## Генерація журналу
```
cd n04_state_event_logs/scripts
python3 generate_event_logs.py --variant 13 --rows 500000
```
Очікуваний файл: `n04_state_event_logs/generated/variant_13_scooter_hub.csv`.

## Запуск
```
sbt "run n04_state_event_logs/generated/variant_13_scooter_hub.csv"
```
Виводить кількість подій і помилок, кількість готових до поїздки самокатів (> 20%),
середній заряд, частку, що потребує підзарядки/сервісу, та порушення інваріантів.

## Тести
```
sbt test
```

## Структура
- `domain.scala` — помилки, smart constructors (`ScooterId`, `Charge`, `Count`), події, стан
- `Parser.scala` — `parseRow` (for-comprehension) і `parseBatch` (`partitionMap`)
- `Rules.scala` — `applyEvent`, `replay` (`foldLeft`), розрахунки та інваріанти
- `Main.scala` — точка входу
