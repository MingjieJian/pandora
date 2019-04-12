      subroutine DEODAR
     $(N,GTO,GTN,PHI,COP,EDGTO,IMG)
C
C     Rudolf Loeser, 1989 Jul 18
C---- Edits the original GTN (=GTO), to get final GTN.
C     (This is version 3 of DEODAR.)
C     !DASH
      save
C     !DASH
      real*8 COP, FAC, GTN, GTO, PHI, ZERO
      integer I, IMG, IQOPE, N
      logical EDGTO, lummy
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(262),IQOPE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external PHAEDRA, HI, BYE
C
C               GTO(N), GTN(N), PHI(N), COP(N), IMG(N)
      dimension GTO(*), GTN(*), PHI(*), COP(*), IMG(*)
C
      data FAC /-0.999D0/
C     !EJECT
C
      call HI ('DEODAR')
C     !BEG
      if(IQOPE.le.0) then
        call PHAEDRA (N, GTO, GTN, lummy)
      else
        do 100 I = 1,N
          if((PHI(I)*GTO(I)+COP(I)).lt.ZERO) then
            GTN(I) = FAC*(COP(I)/PHI(I))
          else
            GTN(I) = GTO(I)
          end if
  100   continue
      end if
C
      EDGTO = .false.
      do 101 I = 1,N
        if(GTO(I).eq.GTN(I)) then
          IMG(I) = 0
        else
          IMG(I) = 1
          EDGTO  = .true.
        end if
  101 continue
C     !END
      call BYE ('DEODAR')
C
      return
      end
