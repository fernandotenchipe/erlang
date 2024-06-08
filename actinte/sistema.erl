%%% Archivo: sistema.erl
%%% Descripción: Sistema distribuido de compras en Erlang sin librerías externas
%%% Fernando Tenchipe Morales A01571277
%%% David Alejandro Lozano Arreola A01722728
%%% Gerardo Leiva Diaz A01198479

%%% Módulo del sistema
-module(sistema).
-export([abre_tienda/0, cierra_tienda/0, start_socio/1, suscribir_socio/1, elimina_socio/1, 
    crea_pedido/2,lista_existencias/0, registra_producto/2, elimina_producto/1, 
    modifica_producto/2, lista_socios/0, productos_vendidos/0, socio_loop/1, tienda_loop/5,
     producto_loop/1]).

%% Funciones de la tienda

abre_tienda() ->
    Inventory = [],
    ProductosVendidos = [],
    register(tienda, spawn(sistema, tienda_loop, [[], Inventory, 0, ProductosVendidos, true])).

cierra_tienda() ->
    unregister(tienda).

tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running) ->
    if
        Running ->
            receive
                {suscribir_socio, From, Socio} ->
                    io:format("Recibe: solicitud de suscripción de ~p~n", [Socio]),
                    case lists:member(Socio, Socios) of
                        true ->
                            From ! {respuesta_suscripcion, {error, ya_existe}},
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running);
                        false ->
                            From ! {respuesta_suscripcion, ok},
                            tienda_loop([Socio | Socios], Inventory, OrderCount, ProductosVendidos, Running)
                    end;
                {elimina_socio, From, Socio} ->
                    io:format("Recibe: solicitud de eliminación de ~p~n", [Socio]),
                    case lists:member(Socio, Socios) of
                        true ->
                            From ! {respuesta_eliminacion, ok},
                            tienda_loop(lists:delete(Socio, Socios), Inventory, OrderCount, ProductosVendidos, Running);
                        false ->
                            From ! {respuesta_eliminacion, {error, no_existe}},
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running)
                    end;
                {crea_pedido, From, Socio, ListaDeProductos} ->
                    io:format("Recibe: pedido de ~p con productos ~p~n", [Socio, ListaDeProductos]),
                    case lists:member(Socio, Socios) of
                        true ->
                            {Respuesta, NuevoInventario, ProductosVendidosActualizados} = procesar_pedido(ListaDeProductos, Inventory, ProductosVendidos),
                            From ! {respuesta_pedido, {ok, OrderCount, Respuesta}},
                            tienda_loop(Socios, NuevoInventario, OrderCount + 1, ProductosVendidosActualizados, Running);
                        false ->
                            From ! {respuesta_pedido, {error, no_socio}},
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running)
                    end;
                {lista_existencias, From} ->
                    io:format("Recibe: solicitud de lista de existencias~n"),
                    From ! {respuesta_existencias, Inventory},
                    tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running);
                {registra_producto, Producto, Cantidad} ->
                    io:format("Recibe: registro de producto ~p con cantidad ~p~n", [Producto, Cantidad]),
                    case lists:keyfind(Producto, 1, Inventory) of
                        false ->
                            ProductoPid = spawn(sistema, producto_loop, [{Producto, Cantidad}]),
                            tienda_loop(Socios, [{Producto, ProductoPid} | Inventory], OrderCount, ProductosVendidos, Running);
                        _ ->
                            io:format("Error: producto ~p ya está registrado~n", [Producto]),
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running)
                    end;
                {elimina_producto, Producto} ->
                    io:format("Recibe: solicitud de eliminación de producto ~p~n", [Producto]),
                    case lists:keyfind(Producto, 1, Inventory) of
                        {Producto, ProductoPid} ->
                            ProductoPid ! stop,
                            tienda_loop(Socios, lists:keydelete(Producto, 1, Inventory), OrderCount, ProductosVendidos, Running);
                        false ->
                            io:format("Error: producto ~p no está registrado~n", [Producto]),
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running)
                    end;
                {modifica_producto, Producto, Cantidad} ->
                    io:format("Recibe: modificación de producto ~p con cantidad ~p~n", [Producto, Cantidad]),
                    case lists:keyfind(Producto, 1, Inventory) of
                        {Producto, ProductoPid} ->
                            ProductoPid ! {modificar, Cantidad},
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running);
                        false ->
                            io:format("Error: producto ~p no está registrado~n", [Producto]),
                            tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running)
                    end;
                {lista_socios, From} ->
                    io:format("Recibe: solicitud de lista de socios~n"),
                    From ! {respuesta_lista_socios, Socios},
                    tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running);
                {productos_vendidos, From} ->
                    io:format("Recibe: solicitud de productos vendidos~n"),
                    From ! {respuesta_productos_vendidos, {OrderCount, ProductosVendidos}},
                    tienda_loop(Socios, Inventory, OrderCount, ProductosVendidos, Running)
            end;
        true ->
            ok
    end.

procesar_pedido(ListaDeProductos, Inventory, ProductosVendidos) ->
    {Respuesta, NuevoInventario, NuevosProductosVendidos} = lists:foldl(
        fun({Producto, Cantidad}, {AccRespuesta, AccInventario, AccProductosVendidos}) ->
            case lists:keyfind(Producto, 1, AccInventario) of
                {Producto, ProductoPid} ->
                    ProductoPid ! {surtir, self(), Cantidad},
                    receive
                        {Producto, Surtido} ->
                            {[{Producto, Surtido} | AccRespuesta],
                             AccInventario,
                             actualizar_productos_vendidos(Producto, Surtido, AccProductosVendidos)}
                    end;
                false ->
                    {[{Producto, 0} | AccRespuesta], AccInventario, AccProductosVendidos}
            end
        end,
        {[], Inventory, ProductosVendidos},
        ListaDeProductos
    ),
    {lists:reverse(Respuesta), NuevoInventario, NuevosProductosVendidos}.

actualizar_productos_vendidos(Producto, Cantidad, ProductosVendidos) ->
    case lists:keyfind(Producto, 1, ProductosVendidos) of
        false ->
            [{Producto, Cantidad} | ProductosVendidos];
        {Producto, CantidadActual} ->
            lists:keyreplace(Producto, 1, ProductosVendidos, {Producto, CantidadActual + Cantidad})
    end.

%% Funciones de los socios

start_socio(TiendaPid) ->
    spawn(sistema, socio_loop, [TiendaPid]).

suscribir_socio(Socio) ->
    TiendaPid = whereis(tienda),
    io:format("Manda: ~p solicita suscripción~n", [Socio]),
    TiendaPid ! {suscribir_socio, self(), Socio},
    receive
        {respuesta_suscripcion, Resultado} ->
            io:format("Recibe: respuesta de suscripción para ~p: ~p~n", [Socio, Resultado]),
            Resultado
    end.

elimina_socio(Socio) ->
    TiendaPid = whereis(tienda),
    io:format("Manda: ~p solicita eliminación de suscripción~n", [Socio]),
    TiendaPid ! {elimina_socio, self(), Socio},
    receive
        {respuesta_eliminacion, Resultado} ->
            io:format("Recibe: respuesta de eliminación para ~p: ~p~n", [Socio, Resultado]),
            Resultado
    end.

crea_pedido(Socio, ListaDeProductos) ->
    TiendaPid = whereis(tienda),
    io:format("Manda: ~p realiza pedido ~p~n", [Socio, ListaDeProductos]),
    TiendaPid ! {crea_pedido, self(), Socio, ListaDeProductos},
    receive
        {respuesta_pedido, {ok, NumeroPedido, ProductosSurtidos}} ->
            io:format("Recibe: pedido número ~p con productos surtidos: ~p~n", [NumeroPedido, ProductosSurtidos]),
            {ok, NumeroPedido, ProductosSurtidos};
        {respuesta_pedido, {error, Motivo}} ->
            io:format("Recibe: error en pedido para ~p: ~p~n", [Socio, Motivo]),
            {error, Motivo}
    end.

lista_existencias() ->
    TiendaPid = whereis(tienda),
    io:format("Manda: solicitud de lista de existencias~n"),
    TiendaPid ! {lista_existencias, self()},
    receive
        {respuesta_existencias, Inventory} ->
            io:format("Recibe: lista de existencias: ~p~n", [Inventory]),
            Inventory
    end.

registra_producto(Producto, Cantidad) ->
    TiendaPid = whereis(tienda),
    io:format("Manda: registrar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
    TiendaPid ! {registra_producto, Producto, Cantidad}.

elimina_producto(Producto) ->
    TiendaPid = whereis(tienda),
    io:format("Manda: eliminar producto ~p~n", [Producto]),
    TiendaPid ! {elimina_producto, Producto}.

modifica_producto(Producto, Cantidad) ->
    TiendaPid = whereis(tienda),
    io:format("Manda: modificar producto ~p con cantidad ~p~n", [Producto, Cantidad]),
    TiendaPid ! {modifica_producto, Producto, Cantidad}.

lista_socios() ->
    TiendaPid = whereis(tienda),
    io:format("Manda: solicitud de lista de socios~n"),
    TiendaPid ! {lista_socios, self()},
    receive
        {respuesta_lista_socios, Socios} ->
            io:format("Recibe: lista de socios: ~p~n", [Socios]),
            Socios
    end.

productos_vendidos() ->
    TiendaPid = whereis(tienda),
    io:format("Manda: solicitud de productos vendidos~n"),
    TiendaPid ! {productos_vendidos, self()},
    receive
        {respuesta_productos_vendidos, {OrderCount, ProductosVendidos}} ->
            io:format("Recibe: pedidos atendidos: ~p, productos vendidos: ~p~n", [OrderCount, ProductosVendidos]),
            {OrderCount, ProductosVendidos}
    end.

socio_loop(TiendaPid) ->
    receive
        {suscribir_socio, Socio} ->
            sistema:suscribir_socio(Socio),
            socio_loop(TiendaPid);
        {elimina_socio, Socio} ->
            sistema:elimina_socio(Socio),
            socio_loop(TiendaPid);
        {crea_pedido, Socio, ListaDeProductos} ->
            sistema:crea_pedido(Socio, ListaDeProductos),
            socio_loop(TiendaPid);
        {lista_existencias} ->
            sistema:lista_existencias(),
            socio_loop(TiendaPid);
        {lista_socios} ->
            sistema:lista_socios(),
            socio_loop(TiendaPid)
    end.

%% Funciones del producto

producto_loop({Producto, Cantidad}) ->
    io:format("Inicia proceso para producto ~p con cantidad ~p~n", [Producto, Cantidad]),
    producto_loop(Producto, Cantidad).

producto_loop(Producto, Cantidad) ->
    receive
        {surtir, From, PedidoCantidad} ->
            Surtido = min(Cantidad, PedidoCantidad),
            io:format("Producto ~p: surte ~p de ~p solicitados~n", [Producto, Surtido, PedidoCantidad]),
            From ! {Producto, Surtido},
            producto_loop(Producto, Cantidad - Surtido);
        {modificar, ModCantidad} ->
            NuevaCantidad = max(Cantidad + ModCantidad, 0),
            io:format("Producto ~p: cantidad modificada a ~p~n", [Producto, NuevaCantidad]),
            producto_loop(Producto, NuevaCantidad);
        stop ->
            io:format("Producto ~p: proceso terminado~n", [Producto])
    end.
