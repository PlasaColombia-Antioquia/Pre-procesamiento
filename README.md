![](logo.jpeg)

# Pre-procesamiento

En este repositorio se encuentra todos los códigos necesarios para crear los insumos del sistema de procesamiento. Entre ellos se encuentra el preprocesamiento de los microdatos, las bases de datos iniciales y el preprocesamiento a las bases de datos de los diferentes tableros.

# Contenido

**01 Códigos data inicial**: Contiene los códigos para el preprocesamiento inicial de los microdatos utilizados para el sistema de abastecimiento de Antioquia.

-   *Abastecimeinto*: Código de preprocesamiento de los datos diarios de abastecimiento del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA), los datos fueron obtenidos de: <https://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-abastecimientos-1>
-   *Precios*: Código de preprocesamiento de los datos diarios de precios en los principales centros de acopio del Sistema de Información de Precios y Abastecimiento del Sector Agropecuario (SIPSA), los datos fueron obtenidos de: <https://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-precios-mayoristas>

**02 Data inicial**: Contiene las bases de datos una vez preprocesados los microdatos, estás serán la base para los indices y bases utilizadas en los tableros.

-   *01_rutas_abastecimiento*: Contiene todas las rutas viales entre los diferentes municipios que cuentan con información de abastecimiento de productos (Fuente: calsulos propios).
-   *02_abastecimiento*: Contiene los datos de abastecimiento mensual con información de municipio de procedencia, municipio destino, producto alimenticio, grupo de producto alimenticio, fecha y las cantidades en kilogramos (Fuente: SIPSA).
-   *03_precios*: Contiene los datos de los precios promedio mensuales en cada uno de las ciudade de los principales centros de acopio, por producto y fecha (Fuente: SIPSA).
-   *04_distancias*: Datos de la distancia en km entre los diferentes muninicipos (Fuente: cálculos propios).
-   *05_municipios*: Datos de los códigos de los diferentes municipios y departamentos (Fuente: DANE).

**03 Códigos tableros**: Contiene los códigos de preprocesamiento de las bases de datos que alimenta cada uno de los distintos tableros del Sistema de Monitoreo de Abastecimiento de Antioquia (SIMONAA). Cada uno de estos códigos toma las bases de data inicial y las transforma según el tablero, creando nuevas medidas, agrupaciones e índices.
