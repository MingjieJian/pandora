      subroutine PHILIP
     $(X,IX,W,IW,ICE,JPR,MORE,LAST,XLB1,SIT,VEC)
C
C     Rudolf Loeser, 2005 May 16
C---- PRD-iterations administration.
C     (This is version 3 of PHILIP.)
C     !DASH
      save
C     !DASH
      real*8 PRDCV, SIT, VEC, W, X, XLB1, dummy
      integer ICE, ITPRD, IW, IX, JPR, MMS, MO, N, jummy
      logical LAST, MORE, SAME
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 66),PRDCV)
      equivalence (KZQ(214),ITPRD)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(26),MMS  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external MOVE1, CONVERD, QUACK, LINER, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), SIT(N,itermax), VEC(N)
      dimension XLB1(*),      SIT(N,*),       VEC(*)
C
      call HI ('PHILIP')
C     !BEG
      MORE = .false.
C
      if(ICE.ne.0) then
C----   Save current S (for iterative behavior analysis)
        call MOVE1      (XLB1(MMS), N, SIT(1,JPR))
C
C----   Check for convergence: set SAME = true if yes
        SAME = .false.
        if(JPR.gt.1) then
          call CONVERD  (SIT(1,JPR-1), 1, N, SIT(1,JPR), 1, N, PRDCV,
     $                   dummy, jummy, SAME)
        end if
C
        if(.not.LAST) then
C----     Do another iteration
          MORE = .true.
          if(SAME.or.(JPR.ge.(ITPRD-1))) then
C----       Mark last iteration: converged, or at limit
            LAST = .true.
          end if
        else
C
C----     The last iteration has been done; now clean up
          if((MO.gt.0).and.(ICE.eq.2)) then
C----       Exhibit iterative behavior
            call QUACK  (MO, N, JPR, SIT, VEC)
C----       Print signoff message
            call LINER  (1, MO)
            if(SAME) then
              write (MO, 100) 'converged', JPR,PRDCV
  100         format(' ','The PRD-iterations ',A,' after ',I2,
     $                   ' iterations; criterion PRDCV =',1PE8.1)
            else
              write (MO, 100) 'did not converge', JPR,PRDCV
            end if
          end if
C
        end if
      end if
C     !END
      call BYE ('PHILIP')
C
      return
      end
