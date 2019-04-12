      subroutine DILBERT
     $(NO,NVX,CVSB,CVXS,CVXM,CVXF,WFB,WTPZ,FBVMX,CVX,WTP,CVZ,CDZ,
     $ FMVLIM)
C
C     Rudolf Loeser, 1997 May 01
C---- Prints input values of velocity parameters.
C     !DASH
      save
C     !DASH
      real*8 CDZ, CVSB, CVX, CVXF, CVXM, CVXS, CVZ, FBVMX, FMVLIM, WFB,
     $       WTP, WTPZ, ZERO
      integer I, IQFLW, NO, NVX
      logical BROD, FLAG, NONZ
      character TVB*44, TVSB*44, TVX*44, TVXS*44
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
      equivalence (IQQ(335),IQFLW)
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
      external NAUGHTD, PADMA, LINER, HI, BYE
C
C               CVX(NVX), WTP(NVX)
      dimension CVX(*),   WTP(*)
C
      data TVSB /'                for Sobolev velocity, CVSB ='/
      data TVXS /'              for expansion velocity, CVXS ='/
      data TVX  /'for additional expansion velocity xx, CVX  ='/
      data TVB  /'     for flow-broadening velocity xx, CVX  ='/
C
      call HI ('DILBERT')
C     !BEG
      if(NO.gt.0) then
        FLAG = .true.
        if(NVX.gt.0) then
          call NAUGHTD (CVX, 1, NVX, FLAG)
        end if
        NONZ = (CVSB.ne.ZERO).or.(CVXS.ne.ZERO)
        NONZ = NONZ.or.(.not.FLAG)
        BROD = IQFLW.gt.0
C
        if(NONZ.or.BROD) then
          call PADMA   (NO, 'Velocity Generating Parameters')
          call LINER   (1, NO)
C
  101     format(' ',8X,A,1PE16.7,:,5X,'profile weight =',E12.4)
          if(CVSB.ne.ZERO) then
            write (NO,101) TVSB,CVSB
          end if
          if(CVXS.ne.ZERO) then
            write (NO,101) TVXS,CVXS
          end if
C
  102     format(I2)
          if((.not.FLAG).or.BROD) then
            do 103 I = 1,NVX
              if(BROD) then
                write (TVB(35:36),102) I
                write (NO,101) TVB,CVX(I),WTP(I)
              else if(.not.FLAG) then
                write (TVX(35:36),102) I
                write (NO,101) TVX,CVX(I)
              end if
  103       continue
          end if
C     !EJECT
          if(BROD) then
            call LINER (1, NO)
            write (NO,104) CVXM,CVXF,FBVMX,WFB,WTPZ
  104       format(' ','For flow broadening: CVXM =',1PE12.4,
     $                 ', CVXF =',E12.4,', FBVMX =', E12.4,', WFB =',
     $                 0PF6.2,', WTPZ =',1PE12.4)
          end if
C
          call LINER   (1, NO)
          write (NO,105) CVZ,CDZ,FMVLIM
  105     format(' ',8X,'CVZ =',1PE15.7,5X,'CDZ =',E15.7,5X,'FMVLIM =',
     $               E15.7)
C
        end if
      end if
C     !END
      call BYE ('DILBERT')
C
      return
      end
