(in-package #:advent-of-code-2024.day09)


(defun parse-input (input)
  (loop
    for i from 0
    for c across input
    for num = (digit-char-p c)
    collect (cons num (if (evenp i) (truncate i 2) nil)) into compact
    if (evenp i)
      sum num into non-nil
    append (make-list num :initial-element (if (evenp i) (truncate i 2) nil))
      into files
    finally (return (values
                     (coerce files 'vector)
                     non-nil
                     compact))))

(defun problem-1 (&key (input *input-part-1-test*))
  (multiple-value-bind (parsed non-nil)
      (parse-input input)
    (loop
      with rev = (loop
                   for el across (reverse parsed)
                   when el
                     collect el)
      for i from 0 below non-nil
      for el across parsed
      sum (* i (or el (pop rev))))))

(defun problem-2 (&key (input *input-part-2-test*))
  )

(defparameter *input-part-1-test*
  "2333133121414131402")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  "7951869951709134922282452546868858728072276373116135929485725576229056321127554484942498608920253277729336399124438846538848957831552147203896149936638237251843478240874014135795169860102115947428164014419681437324361464811846249931644629696816703867169780477745828189523980742227779954667392901880965034366061998678373432629412147795581297776384997775394754701459395198143772397263379674811463849540232488362349598653936699394049172453591244925022892058446761777427442826985372499785989991119370796053662235268897232816721765135397116899564018819273341954632918339796778680906210578688446814211096262816365341804416814593752535524646974224843710533934518530293069222736396163189981227781581992579976906017891437827517449023992212142373251742782489579739706655131389588695738610213383772193783629934995384544406572714144277976112533602286194119752753101478479476914011956369361566724169107389948373897359614898969529573833457070921566634454255458546027608297938137205522129078272861204428388574922820508010317565266695598118973815802787373628942620531054721791756012734456247583625125433946412872256077971746683896909072343268983741146715445395675788528595988359731415426281867564427673414797111742582330896598327095391112951936461052211061774122634365796973273774161051949634262161818747738463902097948840331648286948459539254570334648861843646019847753852538367385976985948960932175598683692913298864959729241158662780172114786731736410494387702342607156146292325641664228636711233497465224538379821855627715607672886832432372739120698049344989989529972122313543425769475063139987539941464726281463136765242617872942573523984519571916892081605130265927553075219578548862465668117139789399407753177190547817375610572456591979579685513734281083141719441024978876813547653598968060265656492997917326399019311750711318223919458388188643613067934225519060994288736535865190748231874956671359932014102879606224839754523085904840772725458710349663873142951665118689424127673294218716532342102257585070854379403894568441809526324673874634843866843161999291287292188828106389304390387658289077352135424944654688198893719824358938387062892827427435256177963753678211645125762279629090409778489637576923947526763131965138137598929216605518712980985932951196674097445510524146554330507518641479969646238484301929939516102630466587129263522771312634487336392237344488385689525677467520867220821615783676225769172662731590931560926529332756338761839654788661579726233954496059624590308696145816752265538416407096273520459469276566556680419726154458427080797824627589347648168557905134235919667583887988948225202789669889449476618433307756474563326380758481414811635533881668886530236129542975475315329217118959837479714046379518444863765057451698673090199385651293676013383413172266792375428166418797441791218010243375193299695957561715698554109199821693462017709084628693852728457130997441365442358034168561111758408968261426161139948645351221961486905510573826832826222053622254721591148948312939744116433914486613931043966635809831189977418048186871333854474459889973203168969838792699449543625016557956452146723487889856788116544771377544435781568959542854106394202812958290482017335085797374609788623467257426961794657723501721677371562757246888468118152567555635102423777464566829843731102098608091652890475250549528258449835755619193322770443391669496289355846493973023771725213657551757938888404428239494798083536048885657878880263727691041288746316337223355738851947843258436974548171790242394771638706290239261713324744332898121283212567599324433492025624478186535664978804913995734206882906481985666945085302684409190183394378975774192492632183354619980987454978438306513318021579899808872441041296664941359284942584519763264773467953012892440984066997243198330618327942755141266729335169227639460607020317574769197431071778168938863685976519061664361749070991553881931597267661381656740601467657649627711174311935416905124746590222429696921339821978250186642539750783614575087598481663864776767424364277964945628913245823276624653838769924877338858263559596222939523194096521631667670383577195062409342618521429628884825933949133425283879157174223051735382317940634573845336298657393066824921669423133798146942869882672258524621567775834071903431932295169366115236607644323757442735369820918632373292456387422772896925686138189542701646445385145028566262106870744634984593774468551766556930585156871760952613125569347948909373292913379332173973451273425947823536878494793542401937591061381971561834302783277887937298314044263576171291145460732792996510544315522020783739653674231027349530826776113542189296654735175423282577445325839193893897849467225330954166594064938016555641244316218936535248338083222437955944617194377040715279806321151241663856501777742698142350845061983539283559954382603181879968573666918046505161648325511749114186636134843937457046833484826543405896682480983353482875995366308287675048716361754753433489326913495294302894483761212068466228404479184633833576939633704272591265672588684174776135895945885826268342302423981779913153596641815966469934511574996285499432711617219175423965225766853773217616487025631663436948875549708331751283245959754030643312786379898150955111617234222567108248579679172140422133647445753633867336449597806123706512754495329524229250225719867836508729245622895573934042231829104656888114329775783054191696822171692753314564953469721240453184189238815581146980851099866368114030743453513858833969209295537958274299515144895144892740889046936554544561352059372440266783775210459750157380902419356177247774655016679737964895618168132980916952435584442040449617576622888313619435518619879225825375897748627920357131659487568530157761362482589787494180832032601151316438855078281131259285108418723829653836654792837370511625946027755818425174602163853232927925613188609266761052725426804898377859287513576848576692836038983365372931356222362269984184642023196292436678732730481353643461411523151288857468868422505274514883344620587163478768509831529469852261155879195017327023677067632270322648414293998225985417779159108110363117648170258089934535306055268294779577948348183072637590407672733841517718655434167380239822289019784537374534278278525830249033106111575890871111253556528464141462942924911110649128423720914953255256872427881893688058941544589394461621834853727159832070824798668711875346889684541732535225268683917685746296639235694354155927906631144112857359577077172349211658879939676617433461536250331610217733995342659523365165391281671552504957563812211271244176379549846094696559879498187182704174163131688132883628688255851284971669602632795612171097667959606067817393201815954917163090885378945682102393579010726518673197584776126637776043192810604153701180922333869815102733116930759024173450452784921347578030651827231957907229943092371417738958671020761317347621475068919628209115611821352315891520859241967439665266142346149440322620103859999529852292781574779575453591923344258464164081565986231655724265531948195467318586404495677917456861638343581377314335973015623174662484196754938059543358604317281129379355223327886542539631765376206323576014433783799457645220468531268059516391435483704993243666352037287026188893851837897359213171581756432436994130275332685172252615517139402332226352142022982149536521558135434824603567425915609571287295108089515862783453549041666173183928501596133944458115631418217816916440166342303731146844682545383947208142344948167823861311314816889775947623308579601622197192267595363057723495245657225920557616693922869658368953898242257284417468657233541246232557404320748611846190299567322662316027858791742389692639687745445951803110635462169063424315492291939944429082404819517349396647493216383811827840551564396553974855353024384850118225814814477216647390348999869752366398236197504124957578832456293678524028355819874410542311136160361444927318853710729926952170491346443511888014936868651263703612317092865115213377546712434353517644943016869536861755598890466124164646344695206910986917817683825880729038264769443537808838458485376687523923274240709752859434562261376626189941815376542115601227536430628195119037715745386330519486158879835063645574724584303781378030451866267129748677201294695966524426159034982497419654985441424354137236885188159591133660927357836694914689489216486628125233963393956661886120145567857031802474267147669577121386306235772845272875994969134597293918417458332363377531769131581190955459408336855063972976226294145419578145896025841311244968547763599082854610872945425776222354634598621846683330204642133550257426798397852724244530829761373642715223203228219921632880119123687326249481827028172540391943582444631297138654422521132330155683668915587656883978327183368215665348969956891495433590996593895524573166446078648775408768926151433836189312301148306270757468749051742828487084345718595022159649191783608813539671702926348562118789283695187692193714269424787614892914927429169578505728366530111723615241299324237471247459445764258639219351298352139889729734758868752163342748475673216687262615808253979385183716339915629435821550462015553642198712345182962541556331815847132493323012577340444887856349959553424567525953748040559410939752577262477240981735271716689736381020967732287333494823797536685759394194801325924252293094394040569118333434415721165457568114735083703678434726232888843580209044374870326677462530224241142539479249931380135442476289244339574154637787804019443013116183218716122992519021597254859766229831398316915959399539191779526071471830339492124383633318944940652340112016322540119427476175439821598931462139429278788791786462948485805454135823816826808854232398162283679997709921222794707762413819131967253831435277896077662593536648694797509928333235453941867789234265936876492437111084345730153884361161831614508038372637554862514230498576462723945892495832847752895998879510368888296015341185585973554590871233883088156530474517302543607037723192841519479372946378936631146748388285925817234134312185559056394667945464893094163557462711274357939257467491358952101559468566112516501237541294253142534779688380169426711090119873702952704471505851643928743172237027307615641825574735124068244761426199135419176682706421926632736462809095758666976259706177467487869060102774156957702329384440191575225410825780401310534387739026703751168073784483268835391524591716916327584570574276291641998437994239464675822262553170279390511167868777754365721594803850271857919151255658159969502428492574715132224147579214401074523564573954557399352317893877386047735921714652803227531124458669887775903327243663267579288333515667258698461295754149802185596645831445479880817477541749996062436173119561881860505487714827214592116782669445733045402652302836311938265189936063761298129212552580175638857617493210553862262785396131142428558153685299189417677199298999261396946861781758151527933762194091742772182868394835124140275430201411584592301873895655214943355874128263894683394076596863957619985674124219606153739076731924626567363457886674543193926992168268997896107045555773868446301091736042951263464590697253577468459875173127269991732149364072235278881255182489301448637721399890743169129644587059197334252074277136446759612026876196563833518074774888848639892186935080158091988955982198986536344162796882111723661475474184186643582215745548706921549333482364194633535929931343886118907416871819289561871975804560698318919581979445714391744355517839995741901473349613662349489944719846734735384110168010509262942239978763733322948899518131323482699024714247634410931236405835535997829829637934684529153299699745108694675277811468797652878384398655518745599294371450397679581214728248563227342082399948453766129759647623926444576889791069322085235750128336787187227732834235538836573852493598632378289457359746981898701969332631261970357120444895376218554860833774537458998697437447758375611198834499304563207161647659525320359525552726367126885460317891809491638566699010749286587782724880717073134352427031876282408517559786313873625415156459176844456080878974908226158052407939608119897595574860183671665494372882501270361748233223192713659467872281238421768311786914758943621082806185623734863890818375584027258858837697915817702533887559762946614868271018146249172232484360631321419691764475297954688420625079614081737548994983581058721259207326688395692664766424536095282892248312191090723595252744219373945619669047958122984578453919274257424957664469991757259366298522209835319254684768496528944825443518613118145316122699777591602988695188491677411888854157321559897024358530276947421071404972711237392560441024886644938267532420384963303589473819285214681420119039293312513541252747152344992996707738543190423619574657987910972491565157401597621953462591673659486765256615246215576785458550162089871451636470718043724828289265691086233619882332613663638046619830158863587288892461852862857980389569985420679565164775823629991762984251206276731164893924123366797641648576868475454136478273372591183632843226805844882463583545657286178210974271697820287238811714787054509697502996552025726456609963398175338259678574265679192110912553986378199394281717876849282967443730712371661292891266192153856317715797765358236178906433384543314438743378744882306985259441158930769631379155969633782176245113104916593880184525517697574576225067216671813922159286317010621261596510405160786787588398799835424966889533187088705469231525182472375740699551487028238614141476139029935362615421686793845211867667251226218410933138451267618995748175113371876914654873653310195672934397847745382385391821171810527914596959994368663863419637188284685158446492752197136455443744784386674173195314785993773044895681105667483157469357864820518639163727322887921152564331178947579264297765411098721115845067401617879468386330616410546975769337567185726856267633781140814090694258479298342947695341998717203628915461415793627838941178655337986198784796372158613195985695953081325658763326339129115730526967656872983395974127787631453050279626141440601638519914319361733651711454795843594268599085605595734092384318534221188632203563471935359762221229588138212783953179963771501047367015647088602095361949743276632252623752732354586268936556608289665357308517191770443110562457658364441268322679545497812931395439497327906550492120832255293161693544767419883056346278489950934228777799658163373358265639523714708030941231158343473411983238204977198685694118514476391385518919284097428385493914217489166248309216102769703250114833828841307314606179466944618578819614309463268066226890597858796465707198134449148928165666846314934166695242699887785262771559852339848935956439697977601696864598797221363353762987409162906843831659238563214049495457578736863175103716814886455750189373297929486584621539351569295196102565335818379793772879482820765456306344102846939596146724577483823740388881249440111236644248577531798641385464654382858896244436716749139279994499797379919714661748991511976837765492553643497925488425131028999229767125209026564090186851383927708975786884855084225498442626135358266756211255448769887469648193771847238964205696589831344053989932512741665011877394734291456865698188309030713035254274433075188811869048992011253734663297226680325486369152987461162929622564308572743355326329111687545632195567252349553137163679192416301458745012909374925736153525641258588427482531919167729237483229949894237088187020876650925473471089722790993955386847878333978036793937335199411275174690336924922187276630115443236328522880323525679592821361124622777571342869207748764115722317263881713343226594433424887251848256878697163151171652291793853428136111875676992726437337131180753093533182111366343789286956747021492615516664555324507741879986258579875382291945849426892560671533427398995629892985954810695838201427985522578524519836968027578458399544609060373883136217569886104612854595499468593324718188959430435377318286656894317226682297745024244597215290953778284974877368189076664533643015835285917997617635339988167640907129107910682684216546633344926680324910624595534895168025669689557413812379684350158816607373979618307826444742189962166296689359492179133425826447405184283469167440106431267383841932551379982811363791739758269511769777479949909499571482377288281445969045167969708379953114972423848168556318424595281512454730193594589532638510128660588721667928604613179899435477655957418963686172581120322129132891951943854113406924214169851642393761493481794898176121777572634428969175654989359368402128383683704127293578534551681143985591693773744738132949541387476323532034663870344449632677107244132089855085464310999967785885276373262055719532483323499490631356505282734762453546644348742576732860422071311642161068672178552370369271962037196534641343668065967631234249898896909929773832422420535042681366525661984590667721648756998387163270941021491614862172751494657581901240374148586673861388975510455548754423947247746368641466673499702153722673916942957353142374196970351531527120919692566939904067448724422532142744745128715363686523353996223589411566759365511337852971245865426784817711314268848449909092319138384148519742571455751781802661523539411597409125496782926058646548258985506284598259632666324717708842731261429522701068171459711147939662643277554385768829295771208178208680946865858758498137883640257715546418132037323495607339517654618041844075343252476920404720881968444553508910332467798856881484532128827158399795374839637630694753146314188495156850913690993930242372326947605871229889948466716416511541848689735314346643529540548757626384577636715380506683513960596598267772274572209157384240278465503691208798681827266991788750822123325679414920161350295664976695957310569563617472719860406768668661543945926568745199493970324941778749993321428442909665869713384879328872474954777717244128922756239218997468599087319129494575422286302678368060468764692687226654346378202074221045231090585394864895229090755889524473934194338099971495404293202278783745687956546166817543365071883423125846839442978028797879686217203399281541592651661586927578213447828861199380118483592059991465347225247165587734652353776015261341307931119998713452931989964346889676623981603536145095674089206054685827391272309829339774815983803653658728929456588361687949111258867690492626944734962988291030614182263461683880795127916546958086758851477711343188269821967452735745292775496215572355862992682787508815786154348125447086185786871172408444759476498377696296957253228353651083194127353711323354698667794071983225247685136858884811979261679094247778767712998420791288252356479293223670469551861453846594244658731961809874135268351361188639531134211930946484757215605750163975548256492331349176463768338718693968799941856546693426177247653716969985119944534852662830512066245944801226625653713657716038392496296790605473395663492419236526226765449423159494562166816190496016352572472584471920476688342212382784661593883250389288186782429976641374519140136396941513322283834617605736622269908851464411763796605638913929755526781657442990934412565469363247884740348538234996228159804633253260944560624741635938215427624012883030515890966795317455815342775890842323671232397711586340767498156071231065355560893429698694947083328128678335634524587968772521468313488385279571815292371277113176606637871480506737696919714463916793743973452480822773309628703411515253219548974756352119513862143341737525642213804238599864751891921274924410877181648574408569153549171697861683937149798357924434243121747321909415332499765947749972612720579095359332283331519317585130908633816881185747451525636853534574592879986051718245645749555943515733133849358674504167542610195098631412651367953963146812341183442646187748168337754712582024642797961482327594867111159488786951472138381426987934736297209055774497769333878043173623604286574586649147105286344824321727979254605566525266254171192815129883973272521922547275971980697887242845968666517755297357711473385998564818721620836238216428583845711322882111798158557627563954519")
