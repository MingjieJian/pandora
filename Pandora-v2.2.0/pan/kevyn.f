      subroutine KEVYN
     $(ISWTCH,KODE,NAME,IU,IL,N,RES,KILROY)
C
C     Rudolf Loeser, 1984 Jan 24
C---- Saves results for radiative transitions.
C     !DASH
      save
C     !DASH
      real*8 RES
      integer IL, ISWTCH, IU, KODE, N
      logical KILROY
      character NAME*(*)
C     !DASH
      external BERWYN, HI, BYE
C
C               RES(N)
      dimension RES(*)
C
      call HI ('KEVYN')
C     !BEG
      if(ISWTCH.gt.0) then
        call BERWYN (KODE, 'Kevyn', NAME, IU, IL, RES, N, KILROY)
      end if
C     !END
      call BYE ('KEVYN')
C
      return
      end
