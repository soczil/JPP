# JPP interpreter (Latte++)

## Zmiany w stosunku do poprzedniej wersji

1. Dodałem nową pętlę for (taką jak w Pascalu), w której wartość kon jest
liczona tylko raz - przed wejściem do pętli. Sposób użycia:
    ```
    for (i = 0 to 10) {
        print(i);
    }
    ```
   
    ```
    for (i = 10 downto 0) {
        print(i);
    }
    ```

2. W Typecheckerze przy sprawdzaniu instrukcji bloku podmieniłem wywołanie
funkcji `checkBlock` na `checkBlockNewEnv`, dzięki czemu przesłanianie
w instrukcji bloku już działa.

## Interpreter

W rozwiązaniu przechowuję stan w monadzie `State`. Stan jest krotką
złożoną ze składu (mapy lokacji na wartości), środowiska (mapy 
identyfikatorów na lokacje), następnej wolnej lokacji, flagi *else* (
która pomaga przy wykonywaniu instrukcji *if _ elif _ else*),
wartość (opakowaną w `Maybe`) zwróconą przez jakąś funkcję) oraz
flagę pętli (również opakowaną w `Maybe`), która informuje o przerwaniu
pętli. Oprócz tego używam też monady `Except` do obsługi błędów wykonania.

## Typechecker

W typecheckerze również używam monady `State` do przechowywania stanu,
czyli krotki zawierającej środowisko (mapę identyfikatorów na informacje
o typie), stare zmienne (zbiór wypełniony identyfikatorami zmiennych,
które można w danym momencie przesłonić) oraz typ obecnie sprawdzanej
funkcji. Tak jak poprzednio, używam monady `Except` do obsługi
błędów - w tym wypadku statycznych.

## Uruchamianie

Wystarczy wywołać polecenie `make`.

Do czyszczenia `make clean`.

Jeśli program nie jest uruchamiany na studentsie, to w Makefilu trzeba
podmienić ścieżkę do `bnfc`.
