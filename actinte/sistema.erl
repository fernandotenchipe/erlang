%%% Archivo: sistema.erl
%%% Descripción: Sistema distribuido de compras en Erlang sin librerías externas
%%% Fernando Tenchipe Morales A01571277
%%% David Alejandro Lozano Arreola A01722728
%%% Gerardo Leiva Diaz A01198479

%%% Módulo del sistema
-module(sistema).
-export([start_tienda/0, start_socio/1, suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0, socio_loop/1, tienda_loop/3]).

%% Funciones de la tienda

start_tienda() ->
    InitialInventory = [
        {producto1, 10},
        {producto2, 20},
        {producto3, 15}
    ],
    register(tienda, spawn(sistema, tienda_loop, [[], InitialInventory, 0])).

tienda_loop(Socios, Inventory, OrderCount) ->
    receive
        {suscribir_socio, From, Socio} ->
            io:format("Recibe: solicitud de suscripción de ~p~n", [Socio]),
            case lists:member(Socio, Socios) of
                true ->
                    From ! {respuesta_suscripcion, {error, ya_existe}},
                    tienda_loop(Socios, Inventory, OrderCount);
                false ->
                    From ! {respuesta_suscripcion, ok},
                    tienda_loop([Socio | Socios], Inventory, OrderCount)
            end;
        {elimina_socio, From, Socio} ->
            io:format("Recibe: solicitud de eliminación de ~p~n", [Socio]),
            case lists:member(Socio, Socios) of
                true ->
                    From ! {respuesta_eliminacion, ok},
                    tienda_loop(lists:delete(Socio, Socios), Inventory, OrderCount);
                false ->
                    From ! {respuesta_eliminacion, {error, no_existe}},
                    tienda_loop(Socios, Inventory, OrderCount)
            end;
        {crea_pedido, From, Socio, ListaDeProductos} ->
            io:format("Recibe: pedido de ~p con productos ~p~n", [Socio, ListaDeProductos]),
            case lists:member(Socio, Socios) of
                true ->
                    {Respuesta, NuevoInventario} = procesar_pedido(ListaDeProductos, Inventory),
                    From ! {respuesta_pedido, {ok, OrderCount, Respuesta}},
                    tienda_loop(Socios, NuevoInventario, OrderCount + 1);
                false ->
                    From ! {respuesta_pedido, {error, no_socio}},
                    tienda_loop(Socios, Inventory, OrderCount)
            end;
        {lista_existencias, From} ->
            io:format("Recibe: solicitud de lista de existencias~n"),
            From ! {respuesta_existencias, Inventory},
            tienda_loop(Socios, Inventory, OrderCount)
    end.

procesar_pedido(ListaDeProductos, Inventory) ->
    {Respuesta, NuevoInventario} = lists:foldl(
        fun({Producto, Cantidad}, {AccRespuesta, AccInventario}) ->
            case lists:keyfind(Producto, 1, AccInventario) of
                {Producto, Stock} when Stock >= Cantidad ->
                    {[{Producto, Cantidad} | AccRespuesta],
                     lists:keyreplace(Producto, 1, AccInventario, {Producto, Stock - Cantidad})};
                {Producto, Stock} ->
                    {[{Producto, Stock} | AccRespuesta],
                     lists:keyreplace(Producto, 1, AccInventario, {Producto, 0})};
                false ->
                    {AccRespuesta, AccInventario}
            end
        end,
        {[], Inventory},
        ListaDeProductos
    ),
    {lists:reverse(Respuesta), NuevoInventario}.

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
        {respuesta_existencias, Inventario} ->
            io:format("Recibe: lista de existencias: ~p~n", [Inventario]),
            Inventario
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
            socio_loop(TiendaPid)
    end.
