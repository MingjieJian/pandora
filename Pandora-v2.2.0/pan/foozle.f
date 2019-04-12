      subroutine FOOZLE
     $(XND,XNK,NL,N,SMALL,OLD,IMG)
C
C     Rudolf Loeser, 2004 Mar 15
C---- Edits "underflow" out of number densities.
C     !DASH
      save
C     !DASH
      real*8 OLD, SMALL, XND, XNK
      integer I, IMG, ISMVE, J, KERM, KMSS, KODE, MODE, N, NERM, NL
      logical BAD
      character LAB*1
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
      equivalence (KZQ(207),ISMVE)
C     !DASH
      external EDITH, SETD, HI, BYE
C
C               XND(N,NL), XNK(N), OLD(N), IMG(N)
      dimension XND(N,*),  XNK(*), OLD(*), IMG(*)
C
      data KODE,MODE,KMSS /2, 1, 0/
      data KERM,NERM      /0, 0/
      data LAB            /' '/
C     !EJECT
C
      call HI ('FOOZLE')
C     !BEG
      if(ISMVE.gt.0) then
C
        do 102 I = 1,N
          BAD = XNK(I).le.SMALL
          if(.not.BAD) then
            do 100 J = 1,NL
              BAD = XND(I,J).le.SMALL
              if(BAD) then
                goto 101
              end if
  100       continue
          end if
  101     continue
C
          if(BAD) then
            XNK(I) = SMALL
            call SETD (XND(I,1), N, NL, SMALL)
          end if
  102   continue
C
        call EDITH    (XNK,      N, SMALL, KODE, MODE, KMSS, LAB, IMG,
     $                 OLD, KERM, NERM, BAD)
        do 103 J = 1,NL
          call EDITH  (XND(1,J), N, SMALL, KODE, MODE, KMSS, LAB, IMG,
     $                 OLD, KERM, NERM, BAD)
  103   continue
C
      end if
C     !END
      call BYE ('FOOZLE')
C
      return
      end
