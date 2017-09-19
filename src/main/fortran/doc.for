c
      SUBROUTINE doc
c _________________________________________________________
c	Program Description
c
c	Doc; A place to store documentation data with the code
c
c _________________________________________________________
C
c,,,,,,
c,Doc.for; Documentation for StateMod,,,,,
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c
c,Common Data,,,,,
c,ndivin,       Div,    Dim,    datinp.f,         i4,   Number of diversions 
c,numdiv,       Div,    Dim,    datinp.f,         i4,   number of diversions
c,
c,maxrtn,       Div,    Dim,    statem.f,         i4,   Number of returns - maximum
c,maxdiv,       Div,    Dim,    statem.f,         i4,   Maximum number of diversions
c,maxdvr,       Div,    Dim,    statem.f,         i4,   maximum number of diversion rights
c,numdvr,       Div,    Dim,    riginp.f,         i4,   number of diversion rights
c
c ___________________________
c,Diversion Data,,,,,
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,area(nd),     Div,    ,       datinp.f read,    f8,   Diversion irrig area
c,
c,cdivid(nd),   Div,    ,       datinp.f read,    a12,  Diversion ID
c,cdividy(nd),  Div,    Daily,  datinp.f read,    a12,  Diversion daily distribution
c,crigid(nr),   Div,    Right,  riginp.f read,    a24   diversion right name
c,
c,divcap(nd),   Div,    ,       datinp.f read,    f8,   Diversion capacity
c,divd(nr),     Div,    Right,
c,diveff(nd),   Div,    Return, datinp.f read,    f8,   Diversion efficiency
c,divmon(nu),   Div,
c,divnam1(nd),  Div,    ,       datinp.f read,    a24,  Diversion name
c,dcrdiv(nr),   Div,    Right,  riginp.f read,    f8,   diversion decree (cfs)
c,
c,___________________________
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,Diversion Demand,,,,,
c,diver(im,nd), Div,    Demand  mdainp.f read,    f8,   Diversion Demand (cfs)
c,diverir(im,nd),Div,   Demand  mdainp.f read,    f8,   IWR Demand (cfs)
c,diwr(im,nd),  Div,    Demand  mdainp.f read,    f8,   IWR Use (cfs)
c,
c,idayd(nd),    Div,    Daily,  datinp.f read,    i4,   Daily data type (   )
c,idivco(1,nr), Div,    Right,  riginp.f,         i4,   ties right to right structure
c,idivsw(nd),   Div,    ,       datinp.f read,    i4,   On/Off for diversion structure
c,idvcom(nd),   Div,    Demand, datinp.f read,    i4,   Diversion demant type
c,idrgst,       Div,    Opr,    oprinp.f read,    i4,   ????
c,
c,idvrsw(nr),   Div,    Right,  riginp.f read,    i4,   On/Off for diversion right
c,idvsta(nd),   Div,    Return, datinp.f read,    i4,   Diversion location on river
c
c ___________________________
c,'Diversion Return Data'
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,maxrtn,        Div,  Return,   Statem.f,         i8,   Maximum number of Diversion returns
c,numRtn,        Div,  Return,   Statem.f,         i8,   Actual number of Diversion returns
c,iPRes(nd),     Div,  Return,   GetDiv.f,         i8,   Diversion return nd is tied to Diversion ipRes(nd)
c,rloss(nd),     Div,  Return,   GetDiv.f,         i8,   Loss from diversion return flow (nd)
c,rlossM(nd),    Div,  Return,   GetDiv.f,         i8,   Monthly total Loss from diversion return flow (nd)
c,nrtn(nd),      Div,  Return,   GetDiv.f,         i8,   Number of diversion return flows for Diversion nd
c,pcttot(nx),    Div,  Return,   GetRrnX.f,        f8,   Percent returning from diversion return nx
c,pctlos(nx),    Div,  Return,   GetDiv.f,         i8,   Monthly total Loss from diversion return nx
c,irtndl(nx),    Div,  Return,   GetDiv.f,        a12,   Stream ID receiving return flow from diversion return nx
c,irnsta(nx),    Div,  Return,   GetDiv.f,         i8,   River Station receiving return flow from diversion return nx
c,
c,irigus,       Div,    Right,  riginp.f,         i4,   ties a right to a user
c,irtn(nd),     Div,    Return, datinp.f read,    i4,   Diversion # of return stations
c,irtndl(n)     Div,    Return  datinp.f read,    i4,   Return Table           
c,irnsta(n),    Div,    Return, datinp.f,         i4,   Return location on stream
c,irnord(n),    Div,    Return, datinp.f read,    i4,   ?? Nummber of nodes downstream of return location????
c,istrtn(n),    Div,    Return, datinp.f          i4,   ?? Return flow ???
c,
c,named(nr),    Div,    Right,  riginp.f read,    a12,  Diversion right name
c,nduser(nd),   Div,    Demand, riginp.f,         i4,   
c,nrigus,       Div,    Right,  riginp.f,         i4,   points a diversion right to a diversion structure
c,nrtn(nd),     Div,    Dim,    datinp.f read,    i4,   Number of returns for diversion nd
c,numuse        Div,    Dim,    datinp.f read,    i4,   Number of users
c,
c,pctot(n),     Div,    Return, datinp.f read,    f8,   Percent returning to a river node
c,
c,rdvnk(nr),    Div,    Right,  riginp.f read,    f16   diversion right admin. number
c,
c ___________________________
c,Plan Data,,,,,
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,maxplan,      Plan,    Dim,    statem.f,         i4,   Maximum Number of plans
c,nplan,        Plan,    Dim,    datinp.f,         i4,   Number of plans
c
c,iPsta(ip),    Plan,    all,    datinp.f,         f8,   Plan stream location
c,iptype(ip),   Plan,    all,    datinp.f,         i8,   Plan type (1=T&C, 2=Well, 3=N/A, 4=Reuse)
c
c,iplnoprE(ip), Plan,    all,    bomsec.f,         i8,   Plan to Operating Rule Pointer for Evap
c,iplnoprR(ip,i),Plan,   all,    bomsec.f,         i8,   Plan to Operating Rule Pointer for Re Diversion
c,iplnoprS(ip,i),Plan,   all,    bomsec.f,         i8,   Plan to Operating Rule Pointer for Plan Source
c,iplnoprU(ip,i),Plan,    all,    bomsec.f,        i8,   Plan to Operating Rule Pointer for Plan Use
c
c,Pon(ip),      Plan,    all,    getpln.f,         f8,   Plan on/off switch
c,Pid(ip),      Plan,    all,    getpln.f,         f8,   Plan ID
c,Pname(ip),    Plan,    all,    getpln.f,         f8,   Plan name
c,Psource(ip),  Plan,    all,    getpln.f,         a12,  Plan source (only used if plan type = 8)
c,iPsource(ip)  Plan,    all,    getpln.f,         i8,   Pointer that ties a plan (np) to a reserovir (only used if the plan type is res recharge (8)
c,Pacct(ip),    Plan,    all,    getpln.f,         a12,  Plan source account (only used if the plan type is reservoir recharge (8)
c,iPlntyp(ip)   Plan,    all,    getpln.f,         i8,   Plan type (1-10 see documentation)
c,
c,Pobl(imo,ip), Plan,    T&C,    datinp.f,         f8,   Plan obligation in month imo
c,Pobld(ido,np),Plan,    T&C,    bomsec.f,         f8,   Plan demand for a given day
c,
c,Pdem(ip),     Plan,    T&C,    bomsec.f,         f8,   Running plan demand (goes up and down)
c,PdemT(ip),    Plan,    T&C,    bomsec.f,         f8,   Total plan demand (goes up only)
c,
c,PdemM(ip),    Plan,    T&C,    bomsec.f,         f8,   Sum of daily running plan demands for a month (dem(ip))
c,PdemTm(ip),   Plan,    T&C,    bomsec.f,         f8,   Sum of daily total plan demands for a given month (demT(ip))
c,
c,Psup(imo,ip), Plan,  Reuse,    datinp.f,         f8,   Reuse Plan supply in month imo
c,PsupD(ido,np),Plan,  Reuse,    datinp.f,         f8,   Reuse plan supply in day ido
c,
c,Psuply(ip),   Plan,  Reuse,    datinp.f,         f8,   Running reuse plan amount (goes up and down)
c,PsuplyT(ip),  Plan,  Reuse,    datinp.f,         f8,   Total reuse plan amount (goes up only)
c,
c,Psuply(ip),   Plan,  Reuse,    bomsec.f,         f8,   Sum of daily running plan demands for a month (dem(ip))
c,PsuplyTm(ip), Plan,  Reuse,    bomsec.f,         f8,   Sum of daily total plan demands for a given month (demT(ip))
c,
c,Peff(ip),     Plan,  N/A,      datinp.f,         f8,   Plan efficiency % (-1 means use sourc structure)
c,iPrg(ip),     Plan,  N/A,      datinp.f,         i8,   Plan return flow pattern (-1 means use sourc structure)
c,iPfail(ip),   Plan,  N/A,      datinp.f,         i8,   Plan failure switch 0 do not stop for a failure, 1 do stop
c,Pmax(im,ip),  Plan,  N/A,      datinp.f,         f8,   Plan maximum diversion for month im  (cfs)
c,
c,PwellP(ip),   Plan,  N/A,      WelRigP.f,        f8,   Well Pumping in Priority
c,PwellPM(ip),  Plan,  N/A,      WelRigP.f,        f8,   Sum of daily Well Pumping in Priority 
c
c,Pdrive(ip),   Plan,  N/A,      WelRigP.f,        f8,   Plan driver (e.g Pumping Exchange, etc.)
c,Pdrive(ip),   Plan,  N/A,      WelRigP.f,        f8,   dum of daily plan driver (e.g Pumping Exchange, etc.)
c
c,iPResOn,      Plan,  N/A,      GetRes.f,         i8,   Reservoir Plan switch (0=off, >0=on)
c
c ___________________________
c,Plan Reporting (Plan to Opr Rule Tie),,,,,
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c
c iplnoprE(ip,io),Plan, NA,     SetPlanO,          i8,  Evap Opr rule for a plan(np) & output(iop) 
c iplnoprS(ip,io),Plan, NA,     SetPlanO,          i8,  Source Opr rule for a plan(np) & output(iop)
c iplnoprR(ip,io),Plan, NA,     SetPlanO,          i8,  Res Re-Diversion Opr rule for plan(np) & output(iop)
c iplnoprO(ip,io),Plan, NA,     SetPlanO,          i8,  OOP Opr rule for a plan(np) & output(iop) 
c iplnoprU(ip,io),Plan, NA,     SetPlanO,          i8,  Use Opr rule for a plan(np) & output(iop) 
c iplnoprP(ip,io),Plan, NA,     SetPlanO,          i8,  Multiple Use Plan for plan(np) & output(iop) 
c
c ___________________________
c,River Data,,,,,
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,avinp(ns),    River,  NA,      datinp.f read,    f8,   Available flow at node ns not adjusted for diversion at node ns
c,avail(ns),    River,  NA,      datinp.f read,    f8,   Available flow at node ns
c,river(ns),    River,  NA,      datinp.f read,    f8,   River flow at node ns

c,qbribu(ns),   River,  NA,      datinp.f read,    f8,   River flow at node ns w/o any return flows
c
c ___________________________                                                 
c,Well Data,,,,,                                                               
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,areaw(nw),     Well, ,         getwel.f read,    f8,   Well irrig area  
c
c,cdividw(nw),   Well, ,         getwel.f read,    a12,  Well ID          
c,cdividyw(nw),  Well, Daily,    getwel.f read,    a12,  Well daily distri
c,crigid(wnr),   Well, Right,    riginp.f read,    a24   Well right name  
c
c,divcapw(nw),   Well, ,         getwel.f read,    f8,   Well capacity    
c,divdw(nr),     Well, Right,                                                  
c,diveffw(nw),   Well, Return,   getwel.f read,    f8,   Well efficiency  
c,divmonw(nw),   Well,                                                         
c,divnamw1(nw),  Well, ,         getwel.f read,    a24,  Well name        
c,dcrdivw(nw),   Well, Right,    riginp.f read,    f8,   Well right (cfs)
c,
c ___________________________                                                                                                
c, Well Demand,,,,,                                                               c
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,diverw(im,nd), Well, Demand    mdainp.f read,    f8,   Well Demand (cfs)
c,diverirw(im,nd),Well,Demand    mdainp.f read,    f8,   Well IWR Demand (cfs)
c,diwrw(im,nd),  Well, Demand    mdainp.f read,    f8,   Well IWR Use (cfs)
c
c ___________________________                                                                                                
c,'Well Return'                                                                                              
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,maxrtnW,      Well,  Return,   Statem.f,         i8,   Maximum number of Well returns                                      
c,numRtnW,      Well,  Return,   Statem.f,         i8,   Actual number of Well returns                                       
c,iPRes(nw),    Well,  Return,   GetWel.f,         i8,   Well return nw is tied to Well ipRes(nd)                            
c,rlossW(nw),   Well,  Return,   GetWel.f,         i8,   Loss from Well return flow (nw)                                     
c,rlossWM(nw),  Well,  Return,   GetWel.f,         i8,   Monthly total Loss from Well return flow (nw)                       
c,nrtnW(nw),    Well,  Return,   GetWel.f,         i8,   Number of Well return flows for Well nw                             
c,pcttotW(nx),  Well,  Return,   GetRrnW.f,        f8,   Percent returning from Well return nx                               
c,pctlosW(nx),  Well,  Return,   GetWel.f,         i8,   Monthly total Loss from Well return nx                              
c,irtndlW(nx),  Well,  Return,   GetWel.f,        a12,   Stream ID receiving return flow from Well return nx                 
c,irnstaW(nx),  Well,  Return,   GetWel.f,         i8,   River Station receiving return flow from Well return nx             
c                                                                                                                            
c ______________ _____________
c,'Well Depletion'
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,maxrtnW2,     Well,  Depletion, Statem.f,         i8,   Maximum number of Well depletions
c,numRtnW2,     Well,  Depletion, Statem.f,         i8,   Actual number of Well depletions
c,iPRes(nw),    Well,  Depletion, GetWel.f,         i8,   Well depletoin nw is tied to Well ipRes(nd)
c,rlossW2(nw),  Well,  Depletion, GetWel.f,         i8,   Salvage from Well depletion flow (nw)
c,rlossWM2(nw), Well,  Depletion, GetWel.f,         i8,   Monthly total salvage from Well depletion flow (nw)
c,nrtnW2(nw),   Well,  Depletion, GetWel.f,         i8,   Number of depletions for Well nw
c,pcttotW2(nx), Well,  Depletion, GetRrnW.f,        f8,   Percent depletions from Well depletion nx
c,pctlosW2(nx), Well,  Depletion, GetWel.f,         i8,   Monthly total salvage from Well depletion nx
c,irtndlW2(nx), Well,  Depletion, GetWel.f,        a12,   Stream ID receiving depletion from Well depletion nx
c,irnstaW2(nx), Well,  Depletion, GetWel.f,         i8,   River Station receiving depletion from Well depletin nx
c
c
c ___________________________
c,'Return flow data for a Reservoir '
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,maxrtnRP,     Plan,  Return,    Statem.f,         i8,   Maximum number of Reservoir returns
c,numRtnRP,     Plan,  Return,    Statem.f,         i8,   Actual number of Reservoir returns
c,iPRes(nr),    Plan,  Return,    GetRes.f,         i8,   Reservoir return nr is tied to plan ipRes(nr)
c,rlossR(nr),   Plan,  Return,    GetRes.f,         i8,   Loss from reservoir return flow (nr)
c,rlossRM(nr),  Plan,  Return,    GetRes.f,         i8,   Monthly total Loss from reservoir return flow (nr)
c,nrtnRP(nr),   Plan,  Return,    GetRes.f,         i8,   Number of reservoir return flows for plan ip
c,pcttotRP(nx), Plan,  Return,    GetRrnX.f,        f8,   Percent returning from reservoir return nx
c,pctlosRP(nx), Plan,  Return,    GetRes.f,         i8,   Monthly total Loss from reservoir return nx
c,irtndlRP(nx), Plan,  Return,    GetRes.f,        a12,   Stream ID receiving return flow from reservoir return nx
c,irnstaRP(nx), Plan,  Return,    GetRes.f,         i8,   River Station receiving return flow from reservoir return nx
c
c ___________________________
c,'Plan flow data for a Plan'
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c,maxrtnPP,     Plan,  N/A,      Statem.f,         i8,   Maximum number of Plan returns
c,numRtnPP,     Plan,  N/A,      Statem.f,         i8,   Actual number of Plan returns
c,iPPln(ip),    Plan,  N/A,      GetPln.f,         i8,   Plan return ip is tied to plan ipPln(nr)
c,rlossP(ip),   Plan,  N/A,      GetPln.f,         i8,   Loss from plan return flow (ip)
c,rlossPM(ip),  Plan,  N/A,      GetPln.f,         i8,   Monthly total Loss from plan return flow (ip)
c,nrtnPP(ip),   Plan,  N/A,      GetPln.f,         i8,   Number of plan return flows for plan ip
c,pcttotPP(nx), Plan,  N/A,      GetRrnX.f,        f8,   Percent returning from plan return nx
c,pctlosPP(nx), Plan,  N/A,      GetRtnX.f,        i8,   Monthly total Loss from plan return nx
c,irtndlPP(nx), Plan,  N/A,      GetRtnX.f,       a12,   Stream ID receiving return flow from plan return nx
c,irnstaPP(nx), Plan,  N/A,      GetRtnX.f,        i8,   River Station receiving return flow from plan return nx
c ___________________________
c,Operational Right Data,,,,,
c,,,,,,
c,Variable,     Cat,    Cat2,   Source,           Type, Description
c,____________ ,______ ,______ ,________________ ,____ ,______________
c
c,divopr,      Opr,    N/A,    OopDiv.f,         f8,   Limit on operational right diversion
c
      RETURN
      END
