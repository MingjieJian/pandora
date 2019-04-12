      subroutine TOSTIG
     $(LU,X,N,TIT)
C
C     Rudolf Loeser, 1983 Aug 15
C---- Saves a sample matrix, for subsequent study.
C     !DASH
      save
C     !DASH
      real*8 X
      integer I, LU, N
      character TIT*50
C     !COM
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
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
      external HI, BYE
C
C               X(N,N)
      dimension X(*)
C
      call HI ('TOSTIG')
C     !BEG
      if(MATSNO.eq.0) then
        rewind LU
        write (LU) HEAD
      end if
      write (LU) N,TIT,(X(I),I=1,(N*N))
      MATSNO = MATSNO+1
C     !END
      call BYE ('TOSTIG')
C
      return
      end
