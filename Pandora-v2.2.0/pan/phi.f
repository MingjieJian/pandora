      subroutine PHI
     $(TAU,IMAX,N,Y,FIN,WH,W)
C
C     Rudolf Loeser, 1989 Jul 13
C---- Computes the "Phi" operator.
C     !DASH
      save
C     !DASH
      real*8 TAU, THREE, TIN, TMS, TOUT, W, WH, Y
      integer IMAX, LUEO, N
      logical FIN
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
      equivalence (RZQ( 44),TMS  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 4),THREE )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PUR, MESHED, ABORT, SECOND, TAFF, HI, BYE
C
      dimension W(*)
C
C               TAU(N,N), WH(N,N)
      dimension TAU(*),   WH(*)
C     !EJECT
C
      call HI ('PHI')
C     !BEG
      if(Y.ne.-THREE) then
        call MESHED ('PHI', 1)
        write (LUEO,100) Y
  100   format(' ','Trouble calculating PHI operator.'//
     $         ' ','The program only does this if the GR method has ',
     $             'been selected; therefore,'/
     $         ' ','Y....., the Source Function Method Selection ',
     $             'parameter, should equal -3; but it''s actual ',
     $             'value is',1PE12.4//
     $         ' ','Please check input. (Are the options EXPAND ',
     $             'and/or SPHERE set as you intended?)')
        call ABORT
      end if
C
      call SECOND   (TIN)
      call PUR      (TAU, IMAX, N, FIN, TMS, WH, W)
      call SECOND   (TOUT)
      call TAFF     (2, TIN, TOUT)
C     !END
      call BYE ('PHI')
C
      return
      end
