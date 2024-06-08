%%% Archivo: sistema_distribuido.erl
%%% Descripción: Sistema distribuido de compras en Erlang sin librerías externas
%%% Fernando Tenchipe Morales A01571277
%%% David Alejandro Lozano Arreola A01722728
%%% Gerardo Leiva Diaz A01198479

%%% Módulo del sistema
-module(sistema).

%%% Exportar las funciones del módulo
-export([abre_tienda/0, cierra_tienda/0, lista_socios/0, productos_vendidos/0,
    suscribir_socio/1, elimina_socio/1, crea_pedido/2, lista_existencias/0,
    registra_producto/2, elimina_producto/1, modifica_producto/2]).

%%% Variable global para la tienda
-define(TIENDA, tienda).

%%% Record para la tienda
-record(tienda, {
    socios = [],
    productos = #{},
    pedidos = [],
    contador_pedidos = 0 }).

%%% Función para abrir la tienda
abre_tienda() ->
    register(?TIENDA, spawn(fun() -> init_tienda(#tienda{}) end)),
    io:format("Tienda abierta~n").

%%% Función para cerrar la tienda
cierra_tienda() ->
    case whereis(?TIENDA) of
        undefined ->
            io:format("La tienda no está abierta~n");
        Pid ->
            Pid ! stop,
            unregister(?TIENDA),
            io:format("Tienda cerrada~n")
    end.

%%% Inicializar el estado de la tienda
init_tienda(State) ->
    process_flag(trap_exit, true),
    tienda(State).

%%% Proceso principal de la tienda
tienda(State) ->
    receive
        stop ->
            io:format("Proceso de tienda terminado~n"),
            exit(normal);
        {From, Request} ->
            spawn(fun() -> handle_request(Request, State, From) end),
            tienda(State)
    end.

%%% Manejar las solicitudes
handle_request({suscribir_socio, Socio}, State, From) ->
    handle_suscribir_socio(Socio, State, From);
handle_request({elimina_socio, Socio}, State, From) ->
    handle_elimina_socio(Socio, State, From);
handle_request({crea_pedido, Socio, Productos}, State, From) ->
    handle_crea_pedido(Socio, Productos, State, From);
handle_request({lista_existencias}, State, From) ->
    handle_lista_existencias(State, From);
handle_request({registra_producto, Producto, Cantidad}, State, From) ->
    handle_registra_producto(Producto, Cantidad, State, From);
handle_request({elimina_producto, Producto}, State, From) ->
    handle_elimina_producto(Producto, State, From);
handle_request({modifica_producto, Producto, Cantidad}, State, From) ->
    handle_modifica_producto(Producto, Cantidad, State, From);
handle_request({lista_socios}, State, From) ->
    handle_lista_socios(State, From);
handle_request({productos_vendidos}, State, From) ->
    handle_productos_vendidos(State, From).

%%% Función para enviar solicitudes a la tienda
call_tienda(Request) ->
    case whereis(?TIENDA) of
        undefined ->
            io:format("La tienda no está abierta~n");
        Pid ->
            Pid ! {self(), Request},
            receive
                Response -> Response
            end
    end.

%%% Funciones de API
suscribir_socio(Socio) ->
    call_tienda({suscribir_socio, Socio}).

elimina_socio(Socio) ->
    call_tienda({elimina_socio, Socio}).

crea_pedido(Socio, Productos) ->
    call_tienda({crea_pedido, Socio, Productos}).

lista_existencias() ->
    call_tienda({lista_existencias}).

registra_producto(Producto, Cantidad) ->
    call_tienda({registra_producto, Producto, Cantidad}).

elimina_producto(Producto) ->
    call_tienda({elimina_producto, Producto}).

modifica_producto(Producto, Cantidad) ->
    call_tienda({modifica_producto, Producto, Cantidad}).

lista_socios() ->
    call_tienda({lista_socios}).

productos_vendidos() ->
    call_tienda({productos_vendidos}).

%%% Implementaciones de manejo de solicitudes

handle_suscribir_socio(Socio, State, From) ->
    io:format("Recibe: solicitud de suscripción de ~p~n", [Socio]),
    case lists:member(Socio, State#tienda.socios) of
        true ->
            From ! {error, "El socio ya está suscrito"},
            tienda(State);
        false ->
            NewState = State#tienda{socios = [Socio | State#tienda.socios]},
            From ! {ok, "Socio suscrito"},
            tienda(NewState)
    end.

handle_elimina_socio(Socio, State, From) ->
    io:format("Recibe: solicitud de eliminación de socio ~p~n", [Socio]),
    NewState = State#tienda{socios = lists:delete(Socio, State#tienda.socios)},
    From ! {ok, "Socio eliminado"},
    tienda(NewState).

handle_crea_pedido(Socio, Productos, State, From) ->
    io:format("Recibe: pedido de ~p con productos ~p~n", [Socio, Productos]),
    case lists:member(Socio, State#tienda.socios) of
        false ->
            From ! {error, "El socio no está suscrito"},
            tienda(State);
        true ->
            NumeroPedido = State#tienda.contador_pedidos + 1,
            {ProductosSurtidos, NuevoInventario} = verificar_inventario(Productos, State#tienda.productos),
            NuevoEstado = State#tienda{
                productos = NuevoInventario,
                pedidos = [{NumeroPedido, Socio, ProductosSurtidos} | State#tienda.pedidos],
                contador_pedidos = NumeroPedido
            },
            From ! {ok, {NumeroPedido, ProductosSurtidos}},
            tienda(NuevoEstado)
    end.

handle_lista_existencias(State, From) ->
    io:format("Existencias: ~p~n", [State#tienda.productos]),
    From ! {ok, State#tienda.productos},
    tienda(State).

handle_registra_producto(Producto, Cantidad, State, From) ->
    io:format("Recibe: registro de producto ~p con cantidad ~p~n", [Producto, Cantidad]),
    Pid = spawn(fun() -> producto(Producto, Cantidad) end),
    NuevoInventario = maps:put(Producto, Pid, State#tienda.productos),
    NuevoEstado = State#tienda{productos = NuevoInventario},
    From ! {ok, "Producto registrado"},
    tienda(NuevoEstado).

handle_elimina_producto(Producto, State, From) ->
    io:format("Recibe: eliminación de producto ~p~n", [Producto]),
    case maps:get(Producto, State#tienda.productos, undefined) of
        undefined ->
            From ! {error, "Producto no encontrado"},
            tienda(State);
        Pid ->
            Pid ! stop,
            NuevoInventario = maps:remove(Producto, State#tienda.productos),
            NuevoEstado = State#tienda{productos = NuevoInventario},
            From ! {ok, "Producto eliminado"},
            tienda(NuevoEstado)
    end.

handle_modifica_producto(Producto, Cantidad, State, From) ->
    io:format("Recibe: modificación de producto ~p con cantidad ~p~n", [Producto, Cantidad]),
    case maps:get(Producto, State#tienda.productos, undefined) of
        undefined ->
            From ! {error, "Producto no encontrado"},
            tienda(State);
        Pid ->
            Pid ! {modifica, Cantidad},
            From ! {ok, "Producto modificado"},
            tienda(State)
    end.

handle_lista_socios(State, From) ->
    io:format("Socios suscritos: ~p~n", [State#tienda.socios]),
    From ! {ok, State#tienda.socios},
    tienda(State).

handle_productos_vendidos(State, From) ->
    io:format("Pedidos atendidos: ~p~n", [State#tienda.pedidos]),
    From ! {ok, State#tienda.pedidos},
    tienda(State).

%%% Proceso de productos
producto(Nombre, Cantidad) ->
    loop_producto(Nombre, Cantidad).

%%% Bucle principal del proceso de productos
loop_producto(Nombre, Cantidad) ->
    receive
        {modifica, CantidadMod} ->
            NuevaCantidad = max(Cantidad + CantidadMod, 0),
            io:format("Producto ~p modificado a cantidad ~p~n", [Nombre, NuevaCantidad]),
            loop_producto(Nombre, NuevaCantidad);
        stop ->
            io:format("Proceso de producto ~p terminado~n", [Nombre]),
            exit(normal);
        {solicitar, CantidadSolicitada, Pid} ->
            CantidadSurtida = min(Cantidad, CantidadSolicitada),
            NuevaCantidad = Cantidad - CantidadSurtida,
            Pid ! {CantidadSurtida, NuevaCantidad},
            loop_producto(Nombre, NuevaCantidad)
    end.

%%% Verificación de inventario
verificar_inventario([], Inventario) ->
    {[], Inventario};
verificar_inventario([{Producto, Cantidad} | Resto], Inventario) ->
    case maps:get(Producto, Inventario, undefined) of
        undefined ->
            {[{Producto, 0} | ProductosRestantes], NuevoInventario} = verificar_inventario(Resto, Inventario),
            {ProductosRestantes, NuevoInventario};
        Pid ->
            Pid ! {solicitar, Cantidad, self()},
            receive
                {CantidadSurtida, NuevaCantidad} ->
                    {[{Producto, CantidadSurtida} | ProductosRestantes], NuevoInventario} = verificar_inventario(Resto, Inventario),
                    {ProductosRestantes, maps:put(Producto, NuevaCantidad, NuevoInventario)}
            end
    end.
