%%% Archivo: sistema.erl
%%% Descripción: Sistema distribuido de compras en Erlang sin librerías externas
%%% Fernando Tenchipe Morales A01571277
%%% David Alejandro Lozano Arreola A01722728
%%% Gerardo Leiva Diaz A01198479


%%%%%  Como usar la tienda
%% Cargar el módulo   | c(sistema).
%% Iniciar            | sistema:abre_tienda().
%% Agregar producto   | sistema:registra_producto(algo, 2).
%% Modificar producto | sistema:modifica_producto(algo, 1).
%% Listar existencias | sistema:lista_existencias(). 
%% Eliminar producto  | sistema:elimina_producto(algo).
%% Cerrar y borrar    | sistema:cierra_tienda().

%%%%%%  Como usar los socios
%% Registrar socio | sistema:suscribir_socio(fulano).
%% Eliminar socio  | sistema:elimina_socio(fulano).
%% Listar socios   | sistema:lista_socios().
%% Crea pedido     | sistema:crea_pedido(fulano, [{nombre, cant}, {nombre1, cant1}]).

%%% Módulo del sistema
-module(sistema).
-export([
    abre_tienda/0, cierra_tienda/0, lista_socios/0, productos_vendidos/0,
    suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0,
    registra_producto/2, elimina_producto/1, modifica_producto/2
]).

%% Registro de un nuevo socio.
suscribir_socio(Socio) ->
    io:format("Manda: ~p solicita suscripción~n", [Socio]),
    tienda ! {suscribir_socio, Socio}.

%% Eliminar socio.
elimina_socio(Socio) -> 
    io:format("Manda: ~p elimina suscripción~n", [Socio]),
    tienda ! {elimina_socio, Socio}.

%% Creación de un pedido.
crea_pedido(Socio, ListaDeProductos) ->
    io:format("Manda: ~p crea pedido ~p~n", [Socio, ListaDeProductos]), 
    tienda ! {crea_pedido, Socio, ListaDeProductos}.

%% Lista de productos en existencia.
lista_existencias() ->
    tienda ! {lista_existencias, self()},
    receive
        {existencias, Existencias} ->
            io:format("Existencias: ~p~n", [Existencias])
    end.

%% Abre la tienda e inicia su proceso.
abre_tienda() ->
    io:format("La tienda esta abierta~n"),
    register(tienda, spawn(fun tienda_loop/0)).

%% Cierra la tienda y termina su proceso.
cierra_tienda() ->
    tienda ! {cierra_tienda}.

%% Lista los socios suscritos.
lista_socios() ->
    tienda ! {lista_socios, self()},
    io:format("Recibe: Ver lista de socios~n"),
    receive
        {socios, Socios} ->
            io:format("Socios: ~p~n", [Socios])
    end.

%% Lista los productos vendidos.
productos_vendidos() ->
    tienda ! {productos_vendidos, self()},
    io:format("Recibe: Ver lista de productos vendidos~n"),
    receive
        {vendidos, Vendidos} ->
            io:format("Productos vendidos: ~p~n", [Vendidos])
    end.

%% Registro de un nuevo producto.
registra_producto(Producto, Cantidad) ->
    tienda ! {registra_producto, Producto, Cantidad}.

%% Elimina un producto activo.
elimina_producto(Producto) ->
    tienda ! {elimina_producto, Producto}.

%% Modifica la cantidad de un producto.
modifica_producto(Producto, Cantidad) ->
    tienda ! {modifica_producto, Producto, Cantidad},
    io:format("Modifica: Cantidad de ~p cambia a ~p~n", [Producto, Cantidad]).

%% Loop de la tienda
tienda_loop() ->
    %% Estado inicial
    loop([], [], 0).

loop(Socios, Productos, Contador) ->
    receive
        {registra_producto, Producto, Cantidad} ->
            io:format("Producto ~p ha sido registrado~n", [Producto]),
            loop(Socios, [{Producto, Cantidad} | Productos], Contador);

        {modifica_producto, Producto, Cantidad} ->
            NuevosProductos = [
                if Producto == ProdProducto -> {ProdProducto, ProdCantidad + Cantidad};
                   true -> {ProdProducto, ProdCantidad}
                end
                || {ProdProducto, ProdCantidad} <- Productos
            ],
            io:format("Cantidad de ~p ha sido modificada a ~p~n", [Producto, Cantidad]),
            loop(Socios, NuevosProductos, Contador);

        {elimina_producto, Producto} ->
            case lists:keytake(Producto, 1, Productos) of
                {value, {Producto, _}, Resto} ->
                    io:format("Producto ~p ha sido eliminado~n", [Producto]),
                    loop(Socios, Resto, Contador);
                false ->
                    loop(Socios, Productos, Contador)
            end;

        {suscribir_socio, Socio} ->
            case lists:member(Socio, Socios) of
                true ->
                    io:format("Solicitud de suscripción de ~p ha sido rechazada~n", [Socio]),
                    loop(Socios, Productos, Contador);
                false ->
                    io:format("Solicitud de suscripción de ~p ha sido aceptada~n", [Socio]),
                    loop([Socio | Socios], Productos, Contador)
            end;

        {elimina_socio, Socio} ->
            io:format("La suscripción de ~p ha sido eliminada~n", [Socio]),
            loop(lists:delete(Socio, Socios), Productos, Contador);

        {crea_pedido, Socio, ListaDeProductos} ->
            case lists:member(Socio, Socios) of
                true ->
                    io:format("Pedido de ~p ha sido recibido~n", [Socio]),
                    PedidoId = Contador + 1,
                    ListaSurtir = [
                        {Producto, min(Cantidad, ProdCantidad)}
                        || {Producto, Cantidad} <- ListaDeProductos,
                           {ProdProducto, ProdCantidad} <- Productos,
                           Producto =:= ProdProducto
                    ],
                    NuevosProductos = [
                        if Producto == ProdProducto -> {ProdProducto, ProdCantidad - CantidadSurtida};
                           true -> {ProdProducto, ProdCantidad}
                        end
                        || {ProdProducto, ProdCantidad} <- Productos,
                           {Producto, CantidadSurtida} <- ListaSurtir,
                           Producto =:= ProdProducto
                    ],
                    io:format("Pedido ~p ha sido aceptado~n", [{aceptado, PedidoId, ListaSurtir}]),
                    loop(Socios, NuevosProductos, PedidoId);
                false ->
                    io:format("Pedido ~p ha sido rechazado - socio no encontrado~n", [{rechazado, socio_no_encontrado}]),
                    loop(Socios, Productos, Contador)
            end;

        {lista_existencias, From} ->
            From ! {existencias, Productos},
            loop(Socios, Productos, Contador);

        {lista_socios, From} ->
            From ! {socios, Socios},
            loop(Socios, Productos, Contador);

        {productos_vendidos, From} ->
            From ! {vendidos, Productos},
            loop(Socios, Productos, Contador);

        {cierra_tienda} ->
            io:format("La tienda está cerrada~n"),
            ok
    end.
