      subroutine HUGRE
     $(X,NWV,WAVES,YWAVE,WAVMN,WAVMX,IPNT,VEC)
C
C     Rudolf Loeser, 1992 Aug 03
C---- Augments WAVES (if desired) and sorts.
C     !DASH
      save
C     !DASH
      real*8 VEC, WAVES, WAVMN, WAVMX, X, YWAVE, ZERO
      integer IPNT, IQUWT, ISTAR, J, NWV
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
      equivalence (IQQ( 77),IQUWT)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SORT, ARMAND, AVERY, ORDERD, HI, BYE
C
      dimension X(*)
C
C               WAVES(NWV), YWAVE(NWV), VEC(NWV), IPNT(NWV)
      dimension WAVES(*),   YWAVE(*),   VEC(*),   IPNT(*)
C     !EJECT
C
      call HI ('HUGRE')
C     !BEG
      if(NWV.gt.0) then
C
        if((WAVMN.gt.ZERO).and.(WAVMX.gt.WAVMN)) then
C----     Find empty terminal sequence in current WAVES
          ISTAR = NWV+1
          do 100 J = 1,NWV
            ISTAR = ISTAR-1
            if(WAVES(ISTAR).ne.ZERO) then
              go to 101
            end if
  100     continue
          ISTAR = 0
C
  101     continue
          if(ISTAR.lt.(NWV-1)) then
            if(IQUWT.gt.0) then
C----         Add standard rates integrations wavelengths
              call ARMAND (X, WAVES, ISTAR, NWV, WAVMN, WAVMX)
C----         Add limits to waves
              if((NWV-ISTAR).gt.1) then
                WAVES(ISTAR+1) = WAVMN
                WAVES(ISTAR+2) = WAVMX
                ISTAR = ISTAR+2
              end if
            end if
            if(ISTAR.lt.NWV) then
C----         Fill remaining unused slots
              call AVERY  (NWV, ISTAR, WAVES, WAVMN, WAVMX)
            end if
          end if
        end if
C
        if(NWV.gt.1) then
C----     Sort WAVES, and YWAVE along with them
          call SORT       (WAVES, NWV, IPNT, 'Augmented WAVES')
          call ORDERD     (YWAVE, IPNT, NWV, VEC)
        end if
C
      end if
C     !END
      call BYE ('HUGRE')
C
      return
      end
