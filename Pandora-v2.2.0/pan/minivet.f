      subroutine MINIVET
     $(ITAU,N,U,BD1,EMB,DMPI)
C
C     Rudolf Loeser, 2003 Jan 07
C---- Computes H level 1 emission at Lyman edge.
C     (This is version 3 of MINIVET.)
C     !DASH
      save
C     !DASH
      real*8 BD1, CA, CON7, EMB, EU, ONE, U, XNK, XNUK, XNUM, ZERO,
     $       dummy1, dummy2
      integer ITAU, LUEO, N
      logical DMPI, KILROY
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(  9),XNUK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external RIGEL, HYDATA, DIVIDE, HI, BYE
C
C               BD1(N,Limdat(1)), U(N)
      dimension BD1(N,*),         U(*)
C
      data KILROY /.true./
C
      call HI ('MINIVET')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL    (7, CON7)
        if(XNUK.eq.ZERO) then
          call HYDATA (0, XNK, dummy1, dummy2)
        else
          XNK = XNUK
        end if
        CA = CON7*(XNK**3)
      end if
C
      EU   = exp(U(ITAU))
      XNUM = BD1(ITAU,1)*EU-ONE
      call DIVIDE     (CA, XNUM, EMB)
C
      if(DMPI) then
        write (LUEO,100) XNK,CON7,CA,U(ITAU),EU,BD1(ITAU,1),XNUM,EMB
  100   format(' ','NUK  =',1PE16.8,5X,'CON7      =',E16.8,5X,
     $             'CA      =',E16.8/
     $         ' ','U(i) =',  E16.8,5X,'exp(U(i)) =',E16.8,5X,
     $             'BD(i,1) =',E16.8/
     $         ' ','num =',E16.8,5X,'EMB =',E16.8)
      end if
C     !END
      call BYE ('MINIVET')
C
      return
      end
