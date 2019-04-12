      subroutine AUTUN
     $(N,X,FRS,HND,VEX,ABDEL,RML)
C
C     Rudolf Loeser, 1983 May 19
C---- Computes rate of mass loss.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, CMPKM, DEN, DILF, FAC, FOUR, FRS, HEABD, HEMASS,
     $       HND, HYMASS, ONE, PI, R1N, RML, VEX, X, dummy
      integer I, IQSFS, KODE, N
      logical SPHERE
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
      equivalence (RZQ( 23),R1N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 5),FOUR  )
C
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
      equivalence (IQQ( 31),IQSFS)
C     !EJECT
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
      equivalence (TUNI( 1),PI    )
      equivalence (PCON( 8),HYMASS)
      equivalence (PCON( 5),HEMASS)
C     !DASH
      external FRANK, LUNA, HI, BYE
C
      dimension X(*)
C
C               FRS(N), HND(N), VEX(N), RML(N), ABDEL(N)
      dimension FRS(*), HND(*), VEX(*), RML(*), ABDEL(*)
C
      call HI ('AUTUN')
C     !BEG
      SPHERE = IQSFS.gt.0
      FAC    = FOUR*PI*(CMPKM**2)
C
      call FRANK ('HE ',0,HEABD,dummy,dummy,dummy,KODE)
      call LUNA  (X,'HE ',HEABD,ABDEL)
C
      do 100 I = 1,N
        DEN    = HYMASS*HND(I)*(ONE+HEMASS*ABDEL(I))
        RML(I) = DEN*VEX(I)*CMPKM
        if(SPHERE) then
          DILF   = FAC*((R1N*FRS(I))**2)
          RML(I) = RML(I)*DILF
        end if
  100 continue
C     !END
      call BYE ('AUTUN')
C
      return
      end
