      subroutine TIGRIS
     $(X,W,COOL,COOLINT,HEAT,HEATINT)
C
C     Rudolf Loeser, 1982 Apr 21
C---- Controls printing and plotting of Cooling and Heating Rates.
C     !DASH
      save
C     !DASH
      real*8 COOL, COOLINT, HEAT, HEATINT, W, X
      integer IITOTL, IN, IQCCI, IS, ITOTAL, JJTE, JJZ, MOX, N, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 37),JJZ  )
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
      equivalence (IQQ(140),IQCCI)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external TARSUS, ARRSUB, IRMAK, HALYS, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               COOL(N), COOLINT(N), HEAT(N), HEATINT(N)
      dimension COOL(*), COOLINT(*), HEAT(*), HEATINT(*)
C
      dimension   IN(2)
      equivalence
     $(IN( 1),ITOTAL),(IN( 2),IITOTL)
C
      call HI ('TIGRIS')
C     !BEG
C     (Get, and allocate, W allotment)
      call TARSUS   (IN,IS,MOX,'TIGRIS')
C
C---- Compute total rates
      call ARRSUB  (COOL,HEAT,W(ITOTAL),N)
      call ARRSUB  (COOLINT,HEATINT,W(IITOTL),N)
C---- Print summary
      call IRMAK   (NO,N,X(JJZ),X(JJTE),COOL,HEAT,W(ITOTAL),IQCCI,
     $              COOLINT,HEATINT,W(IITOTL))
C---- Plot rates
      call HALYS   (NO,1,N,X(JJTE),COOL,HEAT,W(ITOTAL))
C---- Plot integrated rates
      if(IQCCI.gt.0) then
        call HALYS (NO,2,N,X(JJTE),COOLINT,HEATINT,W(IITOTL))
      end if
C
C     (Give back W allotment)
      call WGIVE   (W,'TIGRIS')
C     !END
      call BYE ('TIGRIS')
C
      return
      end
