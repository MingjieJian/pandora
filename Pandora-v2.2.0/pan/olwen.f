      subroutine OLWEN
     $(LEVEL,NL,KODE,ABDION,XNE,SA,BDI,GMI,XND,SUM,SO)
C
C     Rudolf Loeser, 1980 Aug 12
C---- Computes Number Density of level "LEVEL".
C     KODE=1 means - use Departure Coefficients, i.e. nonLTE;
C     KODE=0 means - do not use Departure Coefficients, i.e. LTE.
C     !DASH
      save
C     !DASH
      real*8 ABDION, B, BD, BDI, DENOM, GMI, ONE, RAT, SA, SO, SUM, XND,
     $       XNE, ZERO
      integer I, J, KODE, LEVEL, N, NL
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
C               SUM(N), SA(N), SO(N), ABDION(N), GMI(N,NSL), BDI(N,NL),
      dimension SUM(*), SA(*), SO(*), ABDION(*), GMI(N,*),   BDI(N,*),
C
C               XND(N), XNE(N)
     $          XND(*), XNE(*)
C     !EJECT
C
      call HI ('OLWEN')
C     !BEG
      B    = ONE
      BD   = ONE
      NLTE = (KODE.eq.1)
C
      do 101 I = 1,N
        if(NLTE) then
          B = BDI(I,LEVEL)
        end if
C
        SUM(I) = ZERO
        do 100 J = 1,NL
          if(NLTE) then
            call DIVIDE   (BDI(I,J), BDI(I,LEVEL), BD)
          end if
          SUM(I) = SUM(I)+BD*GMI(I,J)
  100   continue
C
        SO(I) = (ONE/(XNE(I)*B*SA(I)))+SUM(I)
        DENOM = ABDION(I)*GMI(I,LEVEL)
C
        call DIVIDE       (DENOM, SO(I), RAT)
        if(ESPION) then
          call SNUFFLE    (RAT, ZZLALT, ZZSALT, XND(I))
        else
          XND(I) = RAT
        end if
  101 continue
C     !END
      call BYE ('OLWEN')
C
      return
      end
