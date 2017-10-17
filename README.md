# cowboy_fastcgi
This is a sample application that shows how to call PHP scripts from Erlang.

It uses [cowboy](https://github.com/ninenines/cowboy/) as the web server, and
[erl_fastcgi](https://github.com/marcelog/erl_fastcgi) to call [php-fpm](http://php.net/manual/en/install.fpm.php)
and execute PHP scripts and return the result to the browser.

[Poolboy](https://github.com/devinus/poolboy) is used to create a pool of
erl_fastcgi workers to serve the requests.

## Setting it up
Clone the repo and edit the file [config/sys.config](https://github.com/marcelog/cowboy_fastcgi/blob/master/config/sys.config).
You will need:

* php-fpm up and running (you can read how in the [php manual](http://php.net/manual/en/install.fpm.php)
* A directory where to put your PHP files.

## Build
```bash
make
```

## Run it
```bash
make shell
```

## Test it
Create a file like this one in your **php_fpm_root** directory:
```php
<?php
phpinfo();
```

Head to http://localhost:8080/php-fpm/test.php and you should see the output
of the script.

## License
The source code is released under Apache 2 License.

Check [LICENSE](https://github.com/marcelog/cowboy_fastcgi/blob/master/LICENSE) file for more information.
