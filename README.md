# Praktyczne systemy rozproszone

Projekt realizuje tylko pierwsze dwie funkcjonalonści (tj. bez agregacji). Do tego wykorzystujemy Mongo DB z dwoma shardami po 2 repliki. W bazie trzymamy dwie kolekcje `buys` i `views` obie postaci `{_id: Cookie, tags: Array[UserTag]}`. Obie kolekcje poshardowane są po wartości `Cookie`. Obok bazy danych występuje serwis `front`, która odpowiada za komunikację z bazą danych, i generowanie odpowiedzi na zapytanie o ostatnie tagi.

Podział serwisów na nody:
- `front`: `vm[101-104]`
- `mongo_shard_a`: `vm[105-106]`
- `mongo_shard_b`: `vm[107-108]`
- `mongos`: `vm[109-110]`, element Mongo pośredniczący w komunikacji między klientem i shardami
- `mongo_config`: trzy nody z `vm[101-110]`

### Uwagi

Oczywiście rozwiązanie nie korzysta z informacji do debugowania wysyłanych razem z requestami, ale te wartości są deserializowane. Po deserializacji są ignorowane, ale rozwiązanie nie zadziałałoby, gdyby dostało requesty bez tej informacji. Nie zdążyłem usunąć tego i sprawdzić czy wyszystko dalej działa, a nie byłem pewien czy framework, z którego korzystałem, dobrze sobie radzi, gdy dostaje większe requesty niż oczekuje. 
