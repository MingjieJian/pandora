#
# <- Last updated: Thu Oct 31 13:22:03 2013 -> SGK
#
OBJS = abject.o    contid.o   hartr.o    m3av.o     proxy.o    sexp10.o \
       above.o     conver.o   hector.o   markd.o    pvoigt.o   shim.o \
       aboved.o    convlvd.o  helena.o   marki.o    pxpnt1.o   sing1.o \
       abovel.o    convlvr.o  helenar.o  markr.o    pxpnt2.o   sing2.o \
       aboveld.o   cravi.o    hiccup.o   merged.o   pyra.o     sing3.o \
       absvecd.o   cubint.o   hurt.o     minmaxc.o  qexp1.o    sing4.o \
       adacomp.o   cxpnt1.o   imgncod.o  minmaxd.o  qexp2.o    singa.o \
       adair.o     cxpnt2.o   imgprnt.o  minmaxi.o  qexp3.o    singc.o \
       adamore.o   cxpnt3.o   indarrd.o  minmaxr.o  qexp4.o    singd.o \
       adanext.o   cxpnt4.o   indarri.o  minusd.o   qexp5.o    singi.o \
       adapop.o    cxpntx.o   indarrr.o  minusi.o   qexp6.o    singr.o \
       adapush.o   cypris.o   invers.o   minusr.o   qint.o     sinit.o \
       adatest.o   darrout.o  irrsum.o   mnmxc.o    qlog1.o    siprfx.o \
       aderiv1.o   dasher.o   is_decreasing.o  mnmxd.o    qsum.o     skinny.o \
       amura.o     decomp.o   is_increasing.o  mnmxi.o    quadder.o  slather.o \
       ares.o      defact.o   judgec.o   mnmxr.o    races.o    slinec.o \
       arisod.o    dere.o     judged.o   modrv.o    random.o   slog.o \
       arisor.o    derer.o    judgei.o   move1.o    ranged.o   slog10.o \
       arradd.o    deriv1.o   judger.o   movec.o    rangei.o   slurp.o \
       arrave.o    deriv2.o   kanotc.o   moved.o    ranger.o   smooch.o \
       arrdiv.o    determ.o   kanotp.o   movei.o    rauma.o    smorig.o \
       arrest.o    divsum.o   kgive.o    mover.o    recipro.o  smother.o \
       arrinc.o    divvy.o    kinit.o    msum.o     refill.o   sortdc.o \
       arrmul.o    doubler.o  kiwi.o     mulsum.o   reldiff.o  sortdd.o \
       arrout.o    dpray.o    kiwinit.o  mystat.o   reversc.o  sortdi.o \
       arrsub.o    dprive.o   kkount.o   naughtd.o  reversd.o  sortdr.o \
       arrsum.o    drayson.o  klinec.o   naughti.o  reversi.o  splinter.o \
       baksub.o    drooth.o   klinep.o   naughtr.o  reversr.o  splotc.o \
       banbks.o    dvecout.o  konstc.o   nearly.o   right.o    spower.o \
       bandec.o    dvoigt.o   konstd.o   nearsd.o   riia.o     ssin.o \
       beefy.o     edawson.o  konsti.o   nearud.o   rivac.o    ssqrt.o \
       below.o     edichek.o  konstr.o   negate.o   round.o    starer.o \
       belowd.o    edifind.o  kplotc.o   negater.o  round1.o   steeply.o \
       belowl.o    ediset2.o  kplotp.o   notice.o   roundd.o   subssd.o \
       belowld.o   edit1.o    kprint.o   notless.o  roundr.o   subssi.o \
       bessel.o    edit2.o    ktestp.o   notmore.o  roundx.o   subssr.o \
       blankety.o  editest.o  kubilai.o  nucard.o   rowsum.o   subsud.o \
       boundlo.o   editset.o  kwhatc.o   nuchar.o   ruler.o    subsui.o \
       bounds.o    efract.o   kwhatp.o   nuclas.o   runave.o   subsur.o \
       boundup.o   egaush.o   kwhere.o   nudeal.o   rvoigt.o   sumprod.o \
       bush.o      encoded.o  kxlab.o    nufild.o   sanotc.o   swapd.o \
       bushr.o     encoder.o  labfil.o   nufsa.o    satan.o    swapi.o \
       buttem.o    entier.o   limlo.o    nutmeg.o   satan2.o   swapr.o \
       cavir.o     entierd.o  limup.o    one1.o     scaldet.o  swhatc.o \
       cexpint.o   equal.o    liner.o    orderc.o   scalinv.o  swhere.o \
       chloe.o     facet.o    linint.o   orderd.o   scalit.o   sxlab.o \
       cmprssd.o   factin.o   linintr.o  orderi.o   scalmat.o  tdawson.o \
       cmprssi.o   factor.o   lint.o     orderr.o   scanc.o    tint.o \
       comcos.o    fere.o     lintr.o    parboil.o  scand.o    tridag.o \
       comcosd.o   filler.o   logo.o     parbolr.o  scani.o    trnsps.o \
       comexp.o    fiscal.o   lookall.o  parint.o   scanr.o    unit.o \
       comexpd.o   fivedag.o  looksc.o   parintr.o  schema.o   unitadd.o \
       compact.o   fixit.o    looksd.o   paris.o    scos.o     unitsub.o \
       compc.o     flipsd.o   looksi.o   pexpint.o  sderiv1.o  upshot.o \
       compd.o     flog.o     looksr.o   peytre.o   sderiv2.o  vecout.o \
       compi.o     forge.o    lookuc.o   pint.o     search.o   vicar.o \
       compr.o     fourdag.o  lookud.o   pintr.o    searchr.o  weight.o \
       conadd.o    getee.o    lookui.o   plusd.o    set1.o     within.o \
       condiv.o    giant.o    lookur.o   plusi.o    setc.o     zero1.o \
       conmul.o    glaucon.o  lratio.o   plusr.o    setd.o     zerod.o \
       constd.o    gracias.o  lsum.o     pray.o     seti.o     zeroi.o \
       consub.o    guyot.o    lundet.o   privet.o   setr.o     zeror.o \
       contdi.o    hart.o     lvoigt.o   privets.o  sexp.o     zint.o 
#
zoolib.a: $(OBJS)
	ar r $@ $(OBJS)
#
clean:
	-rm *.o
cleaner:
	-rm *.a *.o
