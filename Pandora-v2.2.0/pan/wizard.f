      subroutine WIZARD
     $(LURR,XLB1,RKI,RKW,RLI,EP1,EP2,XNK,XND,BDI,FCE,PCE)
C
C     Rudolf Loeser, 1980 Jan 04
C---- Puts 'iterations' data into restart file.
C     !DASH
      save
C     !DASH
      real*8 BDI, EP1, EP2, FCE, PCE, RKI, RKW, RLI, XLB1, XND, XNK
      integer LURR
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
      external WAND, SPELL, CLOTH, HI, BYE
C
C               XLB1(Li1len), RKI(N,NL), RKW(N,NL), RLI(N,NL), PCE(NT),
      dimension XLB1(*),      RKI(*),    RKW(*),    RLI(*),    PCE(*),
C
C               FCE(N,NT), EP1(N), EP2(N), XND(N,NL), BDI(N,NL), XNK(N)
     $          FCE(*),    EP1(*), EP2(*), XND(*),    BDI(*),    XNK(*)
C
      call HI ('WIZARD')
C     !BEG
      write (LURR,100) HEAD
  100 format(A80)
C
C---- Transitions data
      call WAND  (LURR, XLB1, FCE, PCE)
C---- Lyman data
      call SPELL (LURR, RKI, RKW, RLI, EP1, EP2)
C---- Number density data
      call CLOTH (LURR, XNK, XND, BDI)
C     !END
      call BYE ('WIZARD')
C
      return
      end
