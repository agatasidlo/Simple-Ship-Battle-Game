## Ship Battle Game
Celem naszego programu było stworzenie prostej gry w statki. Polega ona na wyznaczeniu na lewej planszy miejsca, w którym znajdują się statki gracza, a następnie zgadywaniu, gdzie znajdują się statki przeciwnika, wybierając dowolne pola na prawej planszy. Grę wygrywa ten gracz, który pierwszy odgadnie pozycje wszystkich statków przeciwnika. W naszym programie rolę jednego z graczy pełni komputer.

## Opis i schemat struktury zadaniowej programu
Diagram czynności dla struktury zadaniowej programu:
![d1](https://i.postimg.cc/cHv7TgWc/diagram.png)

Opis struktury zadaniowej programu:
- **Proces główny** odpowiada za skonfigurowanie interfejsu graficznego użytkownika i narysowanie odpowiednich elementów na ekranie
w zależności od zmiennej **State**, której wartości odpowiednio zmieniają się w trakcie wykonywania programu. Proces ten uruchamia także proces komputera oraz wysyła do niego odpowiednie wiadomości - o wybraniu miejsc na planszy lub strzelaniu w planszę. Odbiera
od procesu komputera odpowiednie dane, które następnie nanosi na planszę, tak aby były one widoczne dla użytkownika. W procesie głównym następuje wykrycie zdarzenia kliknięcia przez gracza, które w zależności od momentu gry i miejsca kliknięcia jest odpowiednio interpretowane. Wyświetlane są również komunikaty informujące o etapie gry dla użytkownika.
- **Proces komputera** odpowiada za odbieranie wiadomości od procesu głównego, przeprowadzeniu odpowiednich operacji na otrzymanych danych i odesłaniu ich z powrotem do procesu głównego. Proces komputera kończy się w momencie zakończenia procesu głównego.

## Informacje o stosowanych pakietach zewnętrznych
W projekcie został użyty pakiet **wxWidgets**, a dokładnie jego wersja dla języka Erlang (**wxErlang**). Jest to biblioteka klas C++ umożliwiająca programowanie aplikacji okienkowych, korzysta z elementów graficznych środowiska, w którym tworzony jest program. W projekcie wykorzystano również moduł **wx_object**.

## Informacje o zastosowaniu specyficznych metod rozwiązania problemu
Jedną ze specyficznych metod rozwiązania problemu w naszym projekcie było zadecydowanie, jakie operacje będzie wykonywał proces poboczny **computer**. W programie głównym **game** umieszczone zostało polecenie **wx:new()**, odpowiadające za skonfigurowanie GUI, które tworzy środowisko graficzne. Aby używać środowiska graficznego **wx** z kilku procesów, należy pobrać aktywne środowisko poleceniem **Env=wx:get_env()**, a następnie umieścić je w nowym procesie poleceniem **wx:set_env(Env)**. Problem ten rozwiązałyśmy w ten sposób, że to proces główny odpowiada za wizualne umieszczenie wyniku gry na obu planszach, a proces poboczny odpowiada za logikę komputera przy wyborze miejsc na swoje statki lub strzelaniu w statki przeciwnika. Wybrane przez siebie pola wysyła do procesu głównego, który rysuje je na planszy.

## Instrukcja obsługi
Do zainstalowania Erlanga w systemie Linux można użyć polecenia:
'sudo apt-get install erlang'

Nasz projekt wymaga również zainstalowania paczek, co można wykonać poleceniami:
'sudo apt-get install erlang-wx'
'sudo apt-get install libwxgtk3.0-dev'
'sudo apt-get install libwxbase3.0-dev'

Aby uruchomić grę, należy najpierw w konsoli wywołać środowisko Erlanga:
'erl'

Skompilować moduł game:
'c(game).'

Skompilować moduł computer:
'c(computer).'

Na końcu uruchomić grę poleceniem:
'game:start_link().'

## Testy, przykłady
Poniższe grafiki przedstawiają przykładowe zrzuty ekranu podczas trwania gry:

![s1](https://i.postimg.cc/vBCrKYHw/game1.png)
![s2](https://i.postimg.cc/3NpX5sd4/game2.png)

## Możliwe rozszerzenia programu
Program można rozbudować na wiele sposobów. Możliwe jest utworzenie wariantu gry dla dwóch graczy, które wymagałoby użycia 'socket-ów'. W naszym projekcie jeden statek umiejscowiony jest na jednym polu, jednak można również zmodyfikować grę tak, aby umożliwiała ustawianie oraz strzelanie w statki o różnej długości (mieszczące się na kilku polach). Dzięki tej modyfikacji możliwa byłaby zmiana sposobu wyboru przez komputer miejsca na strzał - jego decyzja zależałaby także od analizy dotychczas strzelonych pól na planszy gracza.

##  Ograniczenia programu
Program umożliwia przeprowadzenie jedynie jednej rozgrywki - aby zagrać jeszcze raz, należy powtórnie uruchomić program.

##  Inne informacje
Przy tworzeniu projektu korzystałyśmy z następujących źródeł:

- Erlang/OTP 21.2. Dokumentacja języka Erlang. (http://erlang.org/doc/se)
- wxErlang Reference ManualVersion 1.8.6 December 10, 2018. (http://erlang.org/doc/apps/wx/index.html)
- wxErlang - Speeding Up. Artykuł oraz przykładowa gra w szachy. (https://arifishaq.files.wordpress.com/2018/04/wxerlang-speeding-up.pdf)