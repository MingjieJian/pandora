      subroutine AGATHA
     $(LU)
C
C     Rudolf Loeser, 1983 Aug 15
C---- Prints matrix elements range data.
C     !DASH
      save
C     !DASH
      real*8 SMATC
      integer I, LU, LUSM, MAMAS
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
      equivalence (RZQ( 77),SMATC)
      equivalence (KZQ( 73),MAMAS)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(27),LUSM )
C
C---- MATMAG      as of 1989 Jan 25
      integer     MSTMAX
      parameter   (MSTMAX=10)
C     (Remember to recompile all users when changing MSTMAX)
      integer     MATKNT,NMAT,MATSNO
      real*8      ELLRGE,ELSMLL,ELRNGE
      character   MATNAM*50
      dimension   ELLRGE(MSTMAX),ELSMLL(MSTMAX),NMAT(MSTMAX),
     $            ELRNGE(MSTMAX),MATNAM(MSTMAX)
      common      /MATMAG1/ MATNAM
      common      /MATMAG2/ MATKNT,NMAT
      common      /MATMAG3/ MATSNO
      common      /MATMAG4/ ELLRGE,ELSMLL,ELRNGE
C     Elements range characteristics of the MSTMAX most extreme
C     matrices, (collected when MAMAS=1).
C     .
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('AGATHA')
C     !BEG
      if((LU.gt.0).and.(MAMAS.gt.0)) then
        call LINER   (3,LU)
        write (LU,100)
  100   format(' ','Matrix elements range data',10X,4('**********')//
     $         ' ',5X,'Largest element',4X,'Smallest element',
     $             2X,'log(ratio)',5X,'Order',2X,'Matrix title')
        call LINER   (1,LU)
        write (LU,101) (ELLRGE(I),ELSMLL(I),ELRNGE(I),NMAT(I),
     $                  MATNAM(I),I=1,MSTMAX)
  101   format(' ',1P2E20.12,0PF12.4,I10,2X,A50)
        if(MATSNO.gt.0) then
          call LINER (2,LU)
          write (LU,102) MATSNO,LUSM,SMATC
  102     format(' ',I4,' Matrices were saved in File',I3,
     $               '; the range selection criterion was',F12.4)
        end if
      end if
C     !END
      call BYE ('AGATHA')
C
      return
      end
