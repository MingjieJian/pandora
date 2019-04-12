      subroutine BOSKE
     $(MN1,M,ITER,KAMB, Z,DEL,FA,GA,AA,BA,CA,DA,EA,FB,GB,AB,BB,CB,DB,
     $ EB,PALBET,IAB,PBETAL,IBA,PBETGM,IBG,PGMBET,IGB,H,CHK,P,ALFA,
     $ BETA,HEND,KBINN,KBOUT,S,RES,XO,PRNTM,G1,ANORM,BNORM,CNORM,
     $ DNORM,ENORM,XMULT,DABN,XNK,XN1)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Prints, for OBELISK.
C     !DASH
      save
C     !DASH
      real*8 AA, AB, ALFA, ANORM, BA, BB, BETA, BNORM, CA, CB, CHK,
     $       CNORM, DA, DB, DEL, DNORM, EA, EB, ENORM, FA, FB, G1, GA,
     $       GB, H, HEND, P, PALBET, PBETAL, PBETGM, PGMBET, RES, S,
     $       XMULT, XN1, XNK, XO, Z
      integer I, IAB, IBA, IBG, IGB, ITER, KAMB, KBINN, KBOUT, LUEO, M,
     $        MN1
      logical DABN, PRNTM
      character BOUND*7, LAB*5, STAT*7
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, PRIVET, PROXY, RHEBAN, HI, BYE
C
C               H(2,2,N), Z(N), G1(N), PALBET(N), PBETAL(N), PBETGM(N),
      dimension H(2,2,*), Z(*), G1(*), PALBET(*), PBETAL(*), PBETGM(*),
C
C               PGMBET(N), ALFA(N), BETA(N), FA(N), GA(N), AA(N), P(N),
     $          PGMBET(*), ALFA(*), BETA(*), FA(*), GA(*), AA(*), P(*),
C
C               EA(N), FB(N), GB(N), AB(N), BB(N), CB(N), DB(N), EB(N),
     $          EA(*), FB(*), GB(*), AB(*), BB(*), CB(*), DB(*), EB(*),
C
C               DA(N), HEND(N), XNK(N), XN1(N), BA(N), CA(N), CHK(2*N),
     $          DA(*), HEND(*), XNK(*), XN1(*), BA(*), CA(*), CHK(*),
C
C               CNORM(N), RES(2*N), S(2*N), DEL(N), ANORM(N), BNORM(N),
     $          CNORM(*), RES(*),   S(*),   DEL(*), ANORM(*), BNORM(*),
C
C               DNORM(N), XMULT(N), XO(2*N,2*N), ENORM(N)
     $          DNORM(*), XMULT(*), XO(M,*),     ENORM(*)
C
      dimension LAB(3), STAT(2)
C
      data LAB  /' ', ' He-I', 'He-II'/
      data STAT /'(input)', '(comp.)'/
C     !EJECT
C
      call HI ('BOSKE')
C     !BEG
      BOUND   = 'no'
      if(KBINN.eq.1) then
        BOUND = 'inflow'
      else if(KBOUT.eq.1) then
        BOUND = 'outflow'
      end if
C
      call LINER (3, LUEO)
      write (LUEO,100) LAB(KAMB), ITER, MN1, BOUND
  100 format(' ','***  S I M U L T A N E O U S   S O L U T I O N   ',
     $           'of {He-I,He-II} for ',A//
     $       ' ','N1-iter',I3,5X,'MN1 =',I5,
     $           5X,A7,' boundary condition used')
C
      call LINER   (2, LUEO)
      write (LUEO,101) LAB(2)
  101 format(' ',A,' quantities'//
     $       ' ',16X,'d',14X,'f',14X,'g',14X,'A',14X,'B',14X,'C',
     $           14X,'D',14X,'E')
      call LINER   (1, LUEO)
      write (LUEO,102) (I,DEL(I),FA(I),GA(I),AA(I),BA(I),CA(I),DA(I),
     $                  EA(I),I=1,MN1)
  102 format(5(' ',I4,1P8E15.7/))
C
      call LINER   (2, LUEO)
      write (LUEO,101) LAB(3)
      call LINER   (1, LUEO)
      write (LUEO,102) (I,DEL(I),FB(I),GB(I),AB(I),BB(I),CB(I),DB(I),
     $                  EB(I),I=1,MN1)
C
      call LINER   (2, LUEO)
      write (LUEO,103) STAT(IAB),STAT(IBA),STAT(IBG),STAT(IGB)
  103 format(' ',11X,'PALBET',9X,'PBETAL',9X,'PBETGM',9X,'PGMBET',
     $           9X,'h(1,1)',9X,'h(1,2)',9X,'h(2,1)',9X,'h(2,2)'/
     $       ' ',10X,A7,8X,A7,8X,A7,8X,A7)
      call LINER   (1, LUEO)
      write (LUEO,102) (I,PALBET(I),PBETAL(I),PBETGM(I),PGMBET(I),
     $                  H(1,1,I),H(1,2,I),H(2,1,I),H(2,2,I),I=1,MN1)
C     !EJECT
      if(PRNTM) then
        call LINER  (2, LUEO)
        write (LUEO,104)
  104   format(' ','Matrix')
        call PROXY  (LUEO, XO, M, M, M)
      end if
C
      call VECOUT   (LUEO, S,   M, 'Right-hand-side')
      call VECOUT   (LUEO, RES, M, 'Computed solution')
      call LINER    (1, LUEO)
      write (LUEO,105)
  105 format(' ','CHK(i): the largest off-diagonal element of ',
     $           'row i of the product of the matrix times its ',
     $           'inverse')
      call PRIVET   (LUEO, CHK, M)
C
      if(DABN) then
        call RHEBAN (MN1, ANORM, BNORM, CNORM, DNORM, ENORM, XMULT,
     $               ALFA, BETA)
      end if
C
      call LINER    (2, LUEO)
      write (LUEO,106)
  106 format(' ',16X,'Z',14X,'p',11X,'alfa',11X,'beta',11X,'HEND',
     $           13X,'G1',9X,'NK-new',9X,'N1-new')
      call LINER    (1, LUEO)
      write (LUEO,102) (I,Z(I),P(I),ALFA(I),BETA(I),HEND(I),G1(I),
     $                  XNK(I),XN1(I),I=1,MN1)
C     !END
      call BYE ('BOSKE')
C
      return
      end
