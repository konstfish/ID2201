HW0: Hello Erlang.
Due 3 Sep by 23:59 Points 0

This assignment is not compulsory but highly recommended for those who need help learning Erlang. We will do some Erlang programming and explore how messages can be sent across a network. If you do not have any problems programming in Erlang, you can safely skip this seminar.

[An Erlang Primer](crash.pdf)

After this assignment, you're expected to know how to work in an Erlang programming environment. You should be able to write, compile and run smaller Erlang programs.

KTH student computer room
If you work with the KTH computers, you cannot run Erlang distributed over the network. You can, however, run several Erlang shells on the same computer and let them communicate with each other. To start Erlang in local distribution mode, you start Erlang as follows:

`erl -sname foo -setcookie secret`

and

`erl -sname bar -setcookie secret`

Now check which name your Erlang node has with the calling node(). It will probably say something like foo@pc9826. Now, if you register a process under the name "q" then you should be able to send a message to this process using the following command:

`{q, 'foo@pc9826'} ! hello.`

Ubuntu and other Linux users
If you're using Ubuntu, you can install Erlang in less than three minutes. However, the repository in, for example, Ubuntu no longer includes the erlang-wx module. We use wxWorks, a graphics package,  for some of the labs, and to get it to work with Erlang, we need the erlang-wx module. The simplest solution is to go to Erlang SolutionsLinks to an external site. and install the latest version of the Erlang systems. When you do this, you might have to resolve some dependencies, but you should be up and running in no time.

Choose your favorite packet manager and search for "erlang". Choose the main erlang packet, and if you want to use Emacs as a development environment, also choose "erlang-mode". The "erlang-doc" packet is also nice since your machine will have the HTML documentation locally.

You will find packets for most platforms at Erlang SolutionsLinks to an external site. if you're running other operating systems. If not, you should be able to compile a system using the source found at www.erlang.org. The Erlang system is ported to most platforms, so it should not be a problem.

