&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

                       
DEF VAR p-time          AS INT NO-UNDO.
DEF VAR p-sec           AS INT NO-UNDO. 
DEF VAR p-sec-m         AS INT NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR c-arquivo       AS CHARACTER        NO-UNDO.
DEF VAR p-seg           AS INT  INITIAL 0   NO-UNDO.
DEF VAR p-status        AS LOG  INITIAL NO  NO-UNDO.
DEF VAR p-vezes         AS INT  INITIAL 1   NO-UNDO.
DEF VAR diret           AS CHAR INITIAL ""  NO-UNDO.

DEF TEMP-TABLE tt-msg                       NO-UNDO
         FIELD it_codigo                    AS  CHAR
         FIELD descricao                    AS  CHAR
         FIELD data_hora                    AS  CHAR.
                                            
DEF BUFFER bb-msg       FOR tt-msg.





{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME br-msg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-msg

/* Definitions for BROWSE br-msg                                        */
&Scoped-define FIELDS-IN-QUERY-br-msg tt-msg.it_codigo tt-msg.descricao tt-msg.data_hora   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-msg   
&Scoped-define SELF-NAME br-msg
&Scoped-define QUERY-STRING-br-msg FOR EACH tt-msg NO-LOCK  BY tt-msg.data_hora
&Scoped-define OPEN-QUERY-br-msg OPEN QUERY {&SELF-NAME} FOR EACH tt-msg NO-LOCK  BY tt-msg.data_hora.
&Scoped-define TABLES-IN-QUERY-br-msg tt-msg
&Scoped-define FIRST-TABLE-IN-QUERY-br-msg tt-msg


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br-msg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-29 RECT-30 b-off b-on BUTTON-1 ~
c-caminho br-msg 
&Scoped-Define DISPLAYED-OBJECTS c-data c-hora c-caminho p-desc-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "" 
     SIZE 27.43 BY 2.92
     FONT 0.

DEFINE VARIABLE c-caminho AS CHARACTER FORMAT "X(256)":U 
     LABEL "Caminho" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE c-data AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-hora AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE p-desc-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE b-off
     SIZE 27 BY 2.92.

DEFINE IMAGE b-on
     SIZE 27.43 BY 2.92.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.29 BY 5.25.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.72 BY 13.13.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-msg FOR 
      tt-msg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-msg wWin _FREEFORM
  QUERY br-msg DISPLAY
      tt-msg.it_codigo       FORMAT "x(3)"              COLUMN-LABEL "Cod"      
tt-msg.descricao       FORMAT "x(35)"             COLUMN-LABEL "Mensagem"
tt-msg.data_hora       FORMAT "x(30)"             COLUMN-LABEL "Data/Hora"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66.29 BY 10.79 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-1 AT ROW 1.58 COL 24.43 WIDGET-ID 60
     c-data AT ROW 2.29 COL 2.72 NO-LABEL WIDGET-ID 2
     c-hora AT ROW 2.33 COL 63.29 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-caminho AT ROW 4.79 COL 11.57 COLON-ALIGNED WIDGET-ID 14
     p-desc-status AT ROW 7.42 COL 25.86 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     br-msg AT ROW 8.58 COL 6.72 WIDGET-ID 200
     "DATA" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.42 COL 6.72 WIDGET-ID 6
     "HORA" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.46 COL 68.72 WIDGET-ID 8
     RECT-29 AT ROW 1.29 COL 1.57 WIDGET-ID 22
     RECT-30 AT ROW 6.83 COL 2.14 WIDGET-ID 24
     b-off AT ROW 1.58 COL 24.86 WIDGET-ID 56
     b-on AT ROW 1.58 COL 24.43 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 19.75 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Testar Conex∆o"
         HEIGHT             = 19.75
         WIDTH              = 80
         MAX-HEIGHT         = 29.42
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 29.42
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB br-msg p-desc-status fMain */
ASSIGN 
       b-off:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       b-on:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN c-data IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN c-hora IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN p-desc-status IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-msg
/* Query rebuild information for BROWSE br-msg
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-msg NO-LOCK  BY tt-msg.data_hora.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-msg */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1.58
       COLUMN          = 58.72
       HEIGHT          = 1.25
       WIDTH           = 5.72
       WIDGET-ID       = 12
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(BUTTON-1:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Testar Conex∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Testar Conex∆o */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-off
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-off wWin
ON MOUSE-SELECT-CLICK OF b-off IN FRAME fMain
DO:
  
    IF p-status <> NO THEN DO:
        ASSIGN p-status =   NO
               p-vezes  =   1.

        ASSIGN  p-desc-status:SCREEN-VALUE IN FRAME {&FRAME-NAME}  =   "Conex∆o N∆o Realizada"
                p-desc-status:BGCOLOR   =   12
                b-off:HIDDEN            = TRUE
                b-on:HIDDEN             = FALSE 
                p-seg                   =   p-seg + 1.

                 ENABLE c-caminho with frame {&frame-name}.

                 CREATE tt-msg.
                 ASSIGN tt-msg.it_codigo   =   STRING(p-seg)  
                        tt-msg.descricao   =   "Conex∆o Interrompida"  
                        tt-msg.data_hora   =   c-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "/" + 
                                               c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME}  
                                               .
                {&OPEN-QUERY-{&BROWSE-NAME}}

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-on
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-on wWin
ON MOUSE-SELECT-CLICK OF b-on IN FRAME fMain
DO:
    IF p-status <> YES  THEN DO:
     
        IF (c-caminho:SCREEN-VALUE <> "") THEN
            ASSIGN  p-status = YES.
    
        ELSE
            MESSAGE "Digite o caminho valido"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-msg
&Scoped-define SELF-NAME br-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-msg wWin
ON ROW-DISPLAY OF br-msg IN FRAME fMain
DO:
    
    IF AVAIL tt-msg THEN 
        IF p-desc-status:BGCOLOR = 4 THEN
    
        ASSIGN  tt-msg.it_codigo:FGCOLOR     IN BROWSE br-msg = 15   
                tt-msg.it_codigo:BGCOLOR     IN BROWSE br-msg = 12
                tt-msg.descricao:FGCOLOR     IN BROWSE br-msg = 15 
                tt-msg.descricao:BGCOLOR     IN BROWSE br-msg = 12
                tt-msg.data_hora:FGCOLOR     IN BROWSE br-msg = 15
                tt-msg.data_hora:BGCOLOR     IN BROWSE br-msg = 12.

    ELSE IF p-desc-status:BGCOLOR = 2 THEN 

        ASSIGN  tt-msg.it_codigo:FGCOLOR     IN BROWSE br-msg = 15   
                tt-msg.it_codigo:BGCOLOR     IN BROWSE br-msg = 2
                tt-msg.descricao:FGCOLOR     IN BROWSE br-msg = 15 
                tt-msg.descricao:BGCOLOR     IN BROWSE br-msg = 2
                tt-msg.data_hora:FGCOLOR     IN BROWSE br-msg = 15
                tt-msg.data_hora:BGCOLOR     IN BROWSE br-msg = 2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain
DO:

    IF p-status = NO THEN DO:

        IF (c-caminho:SCREEN-VALUE <> "") THEN DO:
            ASSIGN  p-status = YES.
            BUTTON-1:LOAD-IMAGE(diret + "\" + "off.jpg").
        END.
        ELSE
            MESSAGE "Digite o caminho valido"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    ELSE DO:

        ASSIGN p-status =   NO
               p-vezes  =   1.

        ASSIGN  p-desc-status:SCREEN-VALUE IN FRAME {&FRAME-NAME}  =   "Conex∆o N∆o Realizada"
                p-desc-status:BGCOLOR   =   12
                b-off:HIDDEN            = TRUE
                b-on:HIDDEN             = FALSE 
                p-seg                   =   p-seg + 1.
                BUTTON-1:LOAD-IMAGE(diret + "\" + "on.jpg").

                ENABLE c-caminho with frame {&frame-name}.

                 CREATE tt-msg.
                 ASSIGN tt-msg.it_codigo   =   STRING(p-seg)  
                        tt-msg.descricao   =   "Conex∆o Interrompida"  
                        tt-msg.data_hora   =   c-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "/" + 
                                               c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
                {&OPEN-QUERY-{&BROWSE-NAME}}

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

RUN pi-atualiza.

RUN pi-conecta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}



    REPEAT ix = 1 TO (NUM-ENTRIES(FILE-INFO:FILE-NAME,'\') - 1):  
        ASSIGN diret    =  diret + '\' + ENTRY(ix, FILE-INFO:FILE-NAME,'\').
    END.
    
    ASSIGN diret        =   SUBSTRING(diret,2,500).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "conex.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "conex.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY c-data c-hora c-caminho p-desc-status 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-29 RECT-30 b-off b-on BUTTON-1 c-caminho br-msg 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza wWin 
PROCEDURE pi-atualiza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME, 'HH:MM:SS').
c-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,'99/99/9999').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-conecta wWin 
PROCEDURE pi-conecta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEF INPUT PARAM p-status  AS LOG. */


    FILE-INFO:FILE-NAME = c-caminho:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
      
        IF p-status <> NO THEN DO: 
            IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
                ASSIGN p-seg = p-seg + 1. 

                ASSIGN  p-desc-status:SCREEN-VALUE IN FRAME {&FRAME-NAME}  =   "Falha de Comunicaá∆o"
                            p-desc-status:BGCOLOR   =  4
                            b-off:HIDDEN            = FALSE
                            b-on:HIDDEN             = TRUE
                            p-vezes                 = p-vezes + 1.
                DISABLE c-caminho with frame {&frame-name}.
        
                    CREATE tt-msg.
                    ASSIGN tt-msg.it_codigo   =     STRING(p-seg)  
                           tt-msg.descricao   =     "Endereáo n∆o encontrado"  
                           tt-msg.data_hora   =     c-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "/" + 
                                                    c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
                    {&OPEN-QUERY-{&BROWSE-NAME}}
            END.

            ELSE DO:

                IF p-vezes <= 1 THEN DO:
                    ASSIGN p-seg = p-seg + 1. 

                    ASSIGN  p-desc-status:SCREEN-VALUE IN FRAME {&FRAME-NAME}  =   "Conex∆o Realizada"
                            p-desc-status:BGCOLOR   =  2
                            b-off:HIDDEN            = FALSE
                            b-on:HIDDEN             = TRUE
                            p-vezes                 = p-vezes + 1.
                    DISABLE c-caminho with frame {&frame-name}.
    
                    CREATE tt-msg.
                    ASSIGN tt-msg.it_codigo   =   STRING(p-seg)  
                           tt-msg.descricao   =   "Conex∆o Estabelecida"  
                           tt-msg.data_hora   =   c-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "/" +
                                                  c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME}. 
                    {&OPEN-QUERY-{&BROWSE-NAME}}

                        FIND FIRST bb-msg NO-LOCK
                            WHERE bb-msg.it_codigo = tt-msg.it_codigo  
                        NO-ERROR. 


                    APPLY "row-display" TO br-msg.

                END.
            END.
        END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

