      subroutine MEDEA
     $(NL,KODE,ABDION,XNE,SA,BDI,GMI,XNK,SUM,SO)
C
C     Rudolf Loeser, 1980 Aug 13
C---- Computes Number Density of ionized atoms.
C     KODE=1 means - use departure coefficients, i.e. nonLTE;
C     KODE=0 means - do not use departure coefficients, i.e. LTE.
C     !DASH
      save
C     !DASH
      real*8 ABDION, B, BDI, GMI, ONE, RAT, SA, SO, SUM, XNE, XNK, ZERO
      integer I, J, KODE, N, NL
      logical NLTE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ALTIMA      as of 2004 Mar 15
      real*8      ZZLALT,ZZSALT
      common      /ALTIMA/ ZZLALT,ZZSALT
C     Extreme values of NK, ND and BD.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- ESPY        as of 2004 May 18
      logical     ESPION
      common      /ESPY/ ESPION
C     "Values range" constraint switch
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, SNUFFLE, HI, BYE
C
C               BDI(N,NL), SO(N), SA(N), GMI(N,NSL), XNK(N), ABDION(N),
      dimension BDI(N,*),  SO(*), SA(*), GMI(N,*),   XNK(*), ABDION(*),
C
C               SUM(N), XNE(N)
     $          SUM(*), XNE(*)
C     !EJECT
C
      call HI ('MEDEA')
C     !BEG
      B    = ONE
      NLTE = (KODE.eq.1)
C
      do 101 I = 1,N
C
        SUM(I) = ZERO
        do 100 J = 1,NL
          if(NLTE) then
            B = BDI(I,J)
          end if
          SUM(I) = SUM(I)+B*GMI(I,J)
  100   continue
C
        SO(I) = ONE+(XNE(I)*SA(I)*SUM(I))
C
        call DIVIDE    (ABDION(I), SO(I), RAT)
        if(ESPION) then
          call SNUFFLE (RAT, ZZLALT, ZZSALT, XNK(I))
        else
          XNK(I) = RAT
        end if
C
  101 continue
C     !END
      call BYE ('MEDEA')
C
      return
      end
