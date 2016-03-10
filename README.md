Запуск сервера
> {ok, Ls} = httpd:start(80).

Остановка сервера
> httpd:stop(Ls).