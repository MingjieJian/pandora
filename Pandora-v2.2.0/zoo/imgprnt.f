      subroutine IMGPRNT
     $(LU,IMG,N,KODE)
C     Rudolf Loeser, 1997 Apr 14
C---- Prints an editing record when KODE = 1, and a smoothing record
C     when KODE = 2; in the form of a compressed array image.
C     !DASH
      save
C     !DASH
      integer I, IMG, J, JE, JS, K, KODE, L, LU, N
      character ALF*1, BAD*1, BLANK*1, DASH*1, LEGEND*13, LINE*124,
     $          MAP*110, NUM*1
C     !DASH
      external  IMGNCOD, RULER
      intrinsic min, max
C
      dimension IMG(N), NUM(10), ALF(26)
C
      data NUM /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'/
      data BAD, BLANK, DASH /'x', ' ', '-'/
      data ALF /'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
     $          'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
     $          'u', 'v', 'w', 'x', 'y', 'z'/
C     !EJECT
C
C     !BEG
      if(LU.gt.0) then
C
        JE = -1
  100   continue
          K  = 0
          L  = JE
          JS = JE+1
          JE = min((JE+100),N)
          do 102 J = 1,10
            do 101 I = 1,10
              L = L+1
              K = K+1
              MAP(K:K) = NUM(J)
              if(L.gt.0) then
                if(IMG(L).gt.0) then
                  if(KODE.eq.1) then
                    MAP(K:K) = BAD
                  else if(KODE.eq.2) then
                    MAP(K:K) = ALF(min(IMG(L),26))
                  end if
                end if
              else
                MAP(K:K) = BLANK
              end if
              if(L.eq.JE) goto 103
  101       continue
            K = K+1
            MAP(K:K) = BLANK
  102     continue
C
  103     continue
C
          JS = max(JS,1)
          call IMGNCOD (JS, JE, LEGEND)
          if(JS.eq.1) then
            call RULER (LU, DASH, (K+14))
          end if
          LINE = LEGEND//BLANK//MAP(:K)
          write (LU,104) LINE
  104     format(' ',A124)
        if(JE.lt.N) goto 100
        call RULER     (LU, DASH, (K+14))
C
      end if
C     !END
C
      return
      end
