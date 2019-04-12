      subroutine GITTAR
     $(K,A,V,VP,W,GII,DUMP,J)
C
C     Rudolf Loeser, 2005 Mar 01
C---- Computes a table of GII(v(i),v',a), 1 .le. i .le. K.
C     !DASH
      save
C     !DASH
      real*8 A, GII, V, VP, W
      integer IGII, J, K, MORD
      logical DUMP, KILROY
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
      equivalence (KZQ(213),IGII )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, GRACIAS, RACES, KRABBE, HI, BYE
C
C               V(K), W(K), GII(K)
      dimension V(*), W(*), GII(*)
C
      data MORD,KILROY /2, .true./
C
      call HI ('GITTAR')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        if((IGII.lt.1).or.(IGII.gt.2)) then
          write (MSSLIN(1),100) IGII
  100     format('IGII =',I12,', which is not 1 or 2.')
          call HALT  ('GITTAR', 1)
        end if
      end if
C
      if(IGII.eq.1) then
        call GRACIAS (K, A, V, VP, MORD, W, GII)
      else
        call RACES   (K, A, V, VP,       W, GII)
      end if
      call KRABBE    (DUMP, K, A, MORD, J, VP, V, GII, MORD)
C     !END
      call BYE ('GITTAR')
C
      return
      end
