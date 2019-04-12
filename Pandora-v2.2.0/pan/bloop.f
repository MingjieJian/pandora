      subroutine BLOOP
     $(N,FNDT,EP,DEL,FNP,INCI)
C
C     Rudolf Loeser, 2006 Feb 28
C---- Sets up modified FNDT for printing, for PSHAW.
C     !DASH
      save
C     !DASH
      real*8 DEL, EP, FNDT, FNP, ZERO
      integer I, IQINC, N
      logical INCI
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
      equivalence (IQQ( 51),IQINC)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ZERO1, DIVIDE, HI, BYE
C
C               FNDT(N), EP(N), DEL(N), FNP(N)
      dimension FNDT(*), EP(*), DEL(*), FNP(*)
C
      call HI ('BLOOP')
C     !BEG
      call ZERO1        (FNP, N)
C
      INCI = IQINC.gt.0
C
      if(INCI) then
        do 100 I = 1,N
          if(DEL(I).ne.ZERO) then
            call DIVIDE (FNDT(I), (EP(I)+DEL(I)), FNP(I))
          end if
  100   continue
      end if
C     !END
      call BYE ('BLOOP')
C
      return
      end
