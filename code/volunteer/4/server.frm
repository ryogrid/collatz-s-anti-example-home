[form1.frm]

Option Explicit
Dim mintPortNumber As Integer
Dim mintSeverNumber As Integer
Dim mobjSever(0 To 500) As severclass
Dim mintWithin As Long
Dim mintWaitPort(1000)
Dim muPreserved(50) As Preserved         'クライアントのデータを保持する配列
Dim mintArrangeNumber As Integer          'muPreservedの配列の次に使用すべき添え字
Dim mintNowRange As Integer               '次にどの値を与える必要があるか
Dim strDiscoveredPrime As String
Dim mConfirmedLimit As Integer             '判定された数を保持しておくファイルのレコード数（次に使用すべき数）
Dim lngPreparePrime(77269, 3) As Long             'クライアントへの配分用指数である素数 ２次元目には、実際の素数と、割り振った回数,素数であるかどうかを保持←true=1 falase=2

Private Sub Command1_Click()    '終了処理
    Unload Me
End Sub

Private Sub Command2_Click()
Dim intFilenumber As Integer
Dim strexist As String
Dim intReturn As Integer
Dim work As Confirmed
     
    intReturn = MsgBox("本当によろしいですか？", vbOKCancel + vbCritical)
    If intReturn = vbOK Then
        Call FileCodeOut("ClientData", xORnumber)
        strexist = Dir(App.Path & "\" & "ClientData")            'ClientDataをクリアする
        If strexist = "ClientData" Then
            Kill App.Path & "\" & "ClientData"
            intFilenumber = FreeFile
            Open App.Path & "\" & "ClientData" For Random As intFilenumber Len = Len(muPreserved(0))         '計算し終わった範囲を書き込む
            Close #intFilenumber
            Call FileCodeIn("ClientData", xORnumber)      'ファイルを暗号化
        End If
        Call FileCodeOut("ConfirmedNumber", xORnumber)
        strexist = Dir(App.Path & "\" & "ConfirmedNumber")            'ComfirmedNumberをクリアする
        If strexist = "ConfirmedNumber" Then
            Kill App.Path & "\" & "ConfirmedNumber"
            intFilenumber = FreeFile
            Open App.Path & "\" & "ConfirmedNumber" For Random As intFilenumber Len = Len(work)         '計算し終わった範囲を書き込む
            Close #intFilenumber
            Call FileCodeIn("ConfirmedNumber", xORnumber)            'ファイルを暗号化
        End If
        MSFlexGrid1.Rows = 1                                         'MSflexGridコントロールをクリアーする
        MSFlexGrid1.Cols = 4
        intReturn = MsgBox("更新結果を反映するため一度終了します", vbOKOnly, "終了")
        End
    End If
End Sub

Private Sub Form_Load()
Dim intFilenumber As Integer
Dim n As Long
        
        Form1.Caption = "Collatz's Anti-Exp＠ｈｏｍｅ Server" & "   #アドレス:" & Winsock1.LocalIP
        StatusBar1.SimpleText = "接続待機中"
        MSFlexGrid1.Rows = 1                                         'MSflexGridコントロールの初期設定
        MSFlexGrid1.Cols = 11
        MSFlexGrid1.ColWidth(-1) = MSFlexGrid1.Width / 4
        MSFlexGrid1.RowHeight(-1) = MSFlexGrid1.Height / 20
        MSFlexGrid1.TextMatrix(0, 0) = "ユーザー名"
        MSFlexGrid1.TextMatrix(0, 1) = "ユーザー番号"
        MSFlexGrid1.TextMatrix(0, 2) = "ＩＰアドレス"
        MSFlexGrid1.TextMatrix(0, 3) = "前回計算した数"
        MSFlexGrid1.TextMatrix(0, 4) = "計算中の数"
        MSFlexGrid1.TextMatrix(0, 5) = "返したワーク数"
        MSFlexGrid1.TextMatrix(0, 6) = "プログラム総起動時間"
        MSFlexGrid1.TextMatrix(0, 7) = "ＣＰＵのクロック数"
        MSFlexGrid1.TextMatrix(0, 8) = "ＣＰＵの種類"
        MSFlexGrid1.TextMatrix(0, 9) = "物理メモリ数"
        MSFlexGrid1.TextMatrix(0, 10) = "メッセージ"
        
        MSFlexGrid1.ColAlignment(-1) = flexAlignCenterCenter         'セルの内容を上下ともに中央揃えで表示する
        
        mintSeverNumber = 1
        mintPortNumber = 1002
        Winsock1.LocalPort = 1001   '接続要求受付ポート番号設定
        Winsock1.Listen   '接続要求待ち
        Clipboard.SetText 5
        n = 1
        Call FileCodeOut("ClientData", xORnumber)            'ファイルを復号化
        intFilenumber = FreeFile
        Open App.Path & "\" & "ClientData" For Random As intFilenumber Len = Len(muPreserved(0))         '計算し終わった範囲を書き込む
            Do While Not EOF(intFilenumber)
                Get #intFilenumber, n, muPreserved(n)        'レジストリーファイルから情報を取得
                  If muPreserved(n).intFileAddres <> 0 Then  'ファイルにデータが含まれている場合
                      Call AddList(muPreserved(n))
                      If Left(muPreserved(n).strCulculated, 4) = "現在初回" Then
                           MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 3) = "現在初回"
                      End If
                      If Left(muPreserved(n).strCpuType, 6) = "secret" Then   'クライアントがスペック情報の公開を許可していない場合
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 7) = "secret"
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 9) = "secret"
                      End If
                  End If
                n = n + 1
            Loop
        Close #intFilenumber
        Call FileCodeIn("ClientData", xORnumber)            'ファイルを暗号化
        mintNowRange = 1
        If muPreserved(1).intNowRange <> 0 Then           'すでに計算された範囲がある場合
                      mintNowRange = muPreserved(1).intNowRange
        End If
        strDiscoveredPrime = muPreserved(1).strDiscoveredPrime
        lngPreparePrime(mintNowRange, 1) = muPreserved(1).intDistributeCount
        mintArrangeNumber = n - 1
        intFilenumber = FreeFile                     'バイナリで保持してある素数を配列に読み込む
        Open "PriparePRIME" For Random As intFilenumber Len = Len(lngPreparePrime(1, 0))
            For n = 1 To 501
                  Get #intFilenumber, n, lngPreparePrime(n, 0)
            Next n
        Close #intFilenumber
        mConfirmedLimit = 1
        Call LoadConfirmed              '現在までの判定結果を配列に代入する
        Call ShowConfirmed               '判定結果を表示する
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
        Call DataSave
End Sub

Private Sub Winsock1_Close()
        Winsock1.Close
        Winsock1.Listen
End Sub

Private Sub Winsock1_ConnectionRequest(ByVal requestid As Long)
        Winsock1.Close
        Winsock1.Accept requestid
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
Dim i As Integer
Dim n As Integer
Dim strDat As String   'strDat=受信データ
Static stcstrDat As String
Dim valsplited As Variant
Dim intClientNumber As Integer

         Winsock1.GetData strDat    'クライアントからデータを受信
         stcstrDat = stcstrDat & strDat
         If Right(strDat, 1) = "#" Then
                stcstrDat = Left(stcstrDat, Len(stcstrDat) - 1) '右端の#を取り除く
                stcstrDat = OutCode(stcstrDat, xORnumber)     'データを復号化する
                valsplited = Split(stcstrDat, "!")                 '！を区切り文字として送信されてきたデータを分割する
                intClientNumber = Val(valsplited(2))
ErrorBack: '←クライアントの保持している情報が一致しない場合初めての接続としてスタートする
                If intClientNumber = 0 Then                     'クライアントが始めての接続の場合０が送られてくるのでその場合
                    muPreserved(mintArrangeNumber).strName = SpaceTrim(CStr(valsplited(0)))
                    StatusBar1.SimpleText = muPreserved(mintArrangeNumber).strName & "からの接続処理中"
                    muPreserved(mintArrangeNumber).strAddres = Winsock1.RemoteHostIP
                    muPreserved(mintArrangeNumber).strCulculated = "現在初回"
                    muPreserved(mintArrangeNumber).intFileAddres = mintArrangeNumber
                    muPreserved(mintArrangeNumber).lngNowCulculate = lngPreparePrime(mintNowRange, 0)
                    muPreserved(intClientNumber).lngNowCulculateArray = mintNowRange    '計算してある指数の保存されている場所も保持しておく
                    lngPreparePrime(mintNowRange, 1) = lngPreparePrime(mintNowRange, 1) + 1        '割り振った回数として１加算
                    If lngPreparePrime(mintNowRange, 1) >= 2 Then            'すでに２回割り振った場合、次の値を割り振るため１加算しておく
                                mintNowRange = mintNowRange + 1
                    End If
                    muPreserved(mintArrangeNumber).lngWorkCounts = 1
                    muPreserved(mintArrangeNumber).strMassageFrom = valsplited(4)
                    muPreserved(mintArrangeNumber).intCpuClock = CInt(valsplited(5))
                    muPreserved(mintArrangeNumber).strCpuType = valsplited(6)
                    muPreserved(mintArrangeNumber).intMemorySize = CInt(valsplited(7))
                    muPreserved(mintArrangeNumber).lngClientWorkingTime = CLng(valsplited(8))
                    n = 1                                            '現在ワーク数で順位がいくつなのか調べる
                    For i = 1 To mintArrangeNumber - 1
                          If muPreserved(mintArrangeNumber).lngWorkCounts > 1 Then        '接続中のクライアントよりワーク数の多いクライアントがいた場合
                                      n = n + 1
                          End If
                    Next i
                    muPreserved(mintArrangeNumber).intPrize = n
                    intClientNumber = mintArrangeNumber
                    Call AddList(muPreserved(mintArrangeNumber))
                    MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 3) = "現在初回"
                    If valsplited(6) = "secret" Then    'クライアントがスペック情報の公開を許可していない場合
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 7) = "secret"
                            MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 9) = "secret"
                    End If
                    mintArrangeNumber = mintArrangeNumber + 1
                Else
                    If muPreserved(intClientNumber).lngNowCulculate = CLng(valsplited(1)) Then     'クライアントの情報が保持している情報と一致していた場合
                        StatusBar1.SimpleText = muPreserved(intClientNumber).strName & "からの接続処理中"
                        muPreserved(intClientNumber).strCulculated = valsplited(1)
                        muPreserved(intClientNumber).lngWorkCounts = muPreserved(intClientNumber).lngWorkCounts + 1
                        muPreserved(intClientNumber).lngNowCulculate = lngPreparePrime(mintNowRange, 0)
                        muPreserved(intClientNumber).lngNowCulculateArray = mintNowRange    '計算してある指数の保存されている場所も保持しておく
                        muPreserved(intClientNumber).lngClientWorkingTime = CLng(valsplited(8))
                        lngPreparePrime(mintNowRange, 1) = lngPreparePrime(mintNowRange, 1) + 1        '割り振った回数として１加算
                        If lngPreparePrime(mintNowRange, 1) >= 2 Then            'すでに２回割り振った場合、次の値を割り振るため１加算しておく
                                mintNowRange = mintNowRange + 1
                        End If
                        n = 1                                            '現在ワーク数で順位がいくつなのか調べる
                        For i = 1 To mintArrangeNumber - 1
                          If muPreserved(mintArrangeNumber).lngWorkCounts > 1 Then        '接続中のクライアントよりワーク数の多いクライアントがいた場合
                                      n = n + 1
                          End If
                        Next i
                        muPreserved(mintArrangeNumber).intPrize = n
                        Call RenewList(muPreserved(intClientNumber), intClientNumber)
                        If valsplited(3) = "1" Then                      'クライアントが素数を発見していた場合
                                strDiscoveredPrime = SpaceTrim(strDiscoveredPrime) & "," & SpaceTrim(muPreserved(intClientNumber).strCulculated)
                                lngPreparePrime(muPreserved(intClientNumber).lngNowCulculateArray, 2) = 1        '配列に素数である事を記録しておく
                                Call SaveConfirmed(muPreserved(intClientNumber).lngNowCulculateArray, 1)         'ファイルに記録しておく
                                MSFlexGrid1.TextMatrix(intClientNumber, 3) = "２＾" & CLng(muPreserved(intClientNumber).strCulculated) & "－１ is prime!!"
                        Else      '素数ではなかった場合
                                lngPreparePrime(muPreserved(intClientNumber).lngNowCulculateArray, 2) = 2        '配列に素数である事を記録しておく
                                Call SaveConfirmed(muPreserved(intClientNumber).lngNowCulculateArray, 2)          'ファイルに記録しておく
                        End If
                        Call ShowConfirmed                '配列の内容をリストボックスに表示する
                    Else     'クライアントの情報が保持している情報と一致しない場合
                          StatusBar1.SimpleText = "クライアントかサーバーの保持している情報に誤りがある可能性があります"
                          intClientNumber = 0                                       'つまり、サーバの保持している情報がリセットされたか、クライアントの情報に異常がある場合
                          GoTo ErrorBack           '最初の接続としてやり直すので上へジャンプする
                    End If
                End If
               
                   Winsock1.SendData InCode("##" & "!" & intClientNumber & "!" & muPreserved(intClientNumber).lngNowCulculate & "!" & muPreserved(intClientNumber).lngWorkCounts & "!" & muPreserved(intClientNumber).intPrize, xORnumber) & "#"       '暗号化して送信する
                    stcstrDat = ""       '静的な変数を再びリセットする
                   StatusBar1.SimpleText = "接続待機中"
         End If
End Sub

Private Sub Winsock1_SendComplete()
        Winsock1.Close
        Winsock1.Listen
End Sub

Private Function AddList(Data As Preserved)                 '構造体を引数に渡し、グリッドコントロールにデータを追加する
     MSFlexGrid1.AddItem SpaceTrim(Data.strName)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 1) = Data.intFileAddres
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 2) = RTrim(Data.strAddres)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 3) = "２＾" & SpaceTrim(Data.strCulculated) & "－１"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 4) = "２^" & Data.lngNowCulculate & "－１"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 5) = Data.lngWorkCounts
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 6) = ChangeTime(Data.lngClientWorkingTime)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 7) = Data.intCpuClock & "Mhz"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 8) = SpaceTrim(Data.strCpuType)
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 9) = Data.intMemorySize & "M"
     MSFlexGrid1.TextMatrix(MSFlexGrid1.Rows - 1, 10) = SpaceTrim(Data.strMassageFrom)
End Function

Private Function RenewList(Data As Preserved, FileNumber As Integer)     '構造体を引数に渡し、グリッドコントロールにデータを追加する(すでに書き込まれている部分を書き換える）
     MSFlexGrid1.TextMatrix(FileNumber, 3) = "２＾" & SpaceTrim(Data.strCulculated) & "－１"
     MSFlexGrid1.TextMatrix(FileNumber, 4) = "２＾" & Data.lngNowCulculate & "－１"
     MSFlexGrid1.TextMatrix(FileNumber, 5) = Data.lngWorkCounts
     MSFlexGrid1.TextMatrix(FileNumber, 6) = ChangeTime(Data.lngClientWorkingTime)
End Function

Private Function DataSave()                              'クライアントの情報を保存する関数
Dim intFilenumber As Integer
Dim n As Integer
Dim i As Integer
    
    intFilenumber = FreeFile
    muPreserved(1).intNowRange = mintNowRange     '一番目のレコードだけに現在探索が終わっている範囲を書き込んでおく
    muPreserved(1).intDistributeCount = lngPreparePrime(mintNowRange - 1, 1)     '何度割り振ったかも保存しておく
    muPreserved(1).strDiscoveredPrime = strDiscoveredPrime
    i = 1
    Call FileCodeOut("ClientData", xORnumber)            'ファイルを復号化
    Open App.Path & "\" & "ClientData" For Random As intFilenumber Len = Len(muPreserved(1))         'この時点での情報を書き込む
       For n = 1 To mintArrangeNumber - 1               '現在保持しているクライアントの情報をファイルに書き込む
          If muPreserved(n).intFileAddres <> 0 Then
              Put #intFilenumber, i, muPreserved(n)
          End If
          i = i + 1
       Next n
    Close #intFilenumber
    Call FileCodeIn("ClientData", xORnumber)            'ファイルを復号化
    
End Function

Private Function SpaceTrim(Data As String) As String        'ランダムアクセスで書き込んだ時に末尾に埋められたスペースを取り除く関数
    SpaceTrim = Trim(Replace(Data, Chr(0), ""))
End Function

Private Sub MSFlexGrid1_MouseMove(Button As Integer, _
                    Shift As Integer, x As Single, y As Single)
       If mintArrangeNumber > 1 Then      'すでにクライアントから１台以上接続があった場合
          With MSFlexGrid1
                 MSFlexGrid1.ToolTipText = .TextMatrix(0, .MouseCol) & "：" & .TextMatrix(.MouseRow, .MouseCol)
          End With
       End If
End Sub

Private Function SaveConfirmed(one As Long, two As Integer)           '確認された数を保存しておく関数、引数１=pripareprimeの１次元目　引数２=1:true or 2:false
Dim Number As Confirmed
Dim intFilenumber As Integer
                  
                  Call FileCodeOut("ConfirmedNumber", xORnumber)            'ファイルを復号化
                  Number.lngPrimeNumber = one
                  Number.intTrueOrFalse = two
                  intFilenumber = FreeFile
                  Open App.Path & "\" & "ConfirmedNumber" For Random As intFilenumber Len = Len(Number)
                          Put #intFilenumber, mConfirmedLimit, Number
                  Close #intFilenumber
                  Call FileCodeIn("ConfirmedNumber", xORnumber)            'ファイルを暗号化
                  mConfirmedLimit = mConfirmedLimit + 1
End Function

Private Function LoadConfirmed()                                          '確認された数を保存しておいたファイルConfirmedNumberから読み込む
Dim Number As Confirmed                                                    '※レコードの１番に総レコード数を保持しておく
Dim intFilenumber As Integer
Dim work As Integer
Dim strexist As String
                  
On Error Resume Next
                  Call FileCodeOut("ConfirmedNumber", xORnumber)            'ファイルを復号化
                  strexist = Dir(App.Path & "\" & "ConfirmedNumber")
                  If strexist = "ConfirmedNumber" Then
                        Call FileCodeOut("ConfirmedNumber", xORnumber)            'ファイルを復号化
                        intFilenumber = FreeFile
                        Open App.Path & "\" & "ConfirmedNumber" For Random As intFilenumber Len = Len(Number)
                        Do While Not EOF(intFilenumber)
                                Get #intFilenumber, work + 1, Number
                                lngPreparePrime(Number.lngPrimeNumber, 2) = Number.intTrueOrFalse
                                work = work + 1
                        Loop
                        Close #intFilenumber
                        Call FileCodeIn("ConfirmedNumber", xORnumber)            'ファイルを暗号化
                        mConfirmedLimit = work + 1         '次に使用すべきレコード番号を保持しておく
                  End If
End Function

Private Function ChangeTime(ByVal Second As Long) As String             '秒数を渡すと、ｘ時間、ｘ分、ｘ秒に直して、文字列として返す関数
Dim Hour As Long
Dim Minute As Long
Dim work As Long
Dim str As String

            Hour = Second \ 3600
            work = Second Mod 3600
            Minute = work \ 60
            Second = work Mod 60
            If Hour <> 0 Then
                  str = Hour & "時間"
            End If
            If Minute <> 0 Then
                  str = str & Minute & "分"
            End If
            If Second <> 0 Then
                  str = str & Second & "秒"
            End If
            ChangeTime = str
End Function

Private Function ShowConfirmed()             'リストボックスコントロールに配列に代入してある判定結果をすべて表示する
Dim i As Integer

                 List1.Clear
                 For i = 1 To mintNowRange - 1
                       Select Case lngPreparePrime(i, 2)
                             Case 0
                                 List1.AddItem "2^" & lngPreparePrime(i, 0) & "-1 is Now Culculate"
                             Case 1
                                 List1.AddItem "2^" & lngPreparePrime(i, 0) & "-1 is Anti-Example?"
                             Case 2
                                 List1.AddItem "2^" & lngPreparePrime(i, 0) & "-1 is OK"
                             End Select
                 Next i
End Function

Private Function InCode(ByVal str As String, xORnumber As Integer) As String           '文字列を渡すと暗号化された文字列を返す（Ｘｏｒ結合で結合する値が必要）
Dim bytArray() As Byte
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            ReDim bytArray(lstrlen(str))
            Call MoveMemory(bytArray(0), ByVal str, lstrlen(str))
            str = ""
            For i = 0 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            For i = 0 To UBound(bytArray)
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
                str = str & "," & bytArray(i)
            Next i
                str = Right(str, Len(str) - 1)
                InCode = str
End Function

Private Function OutCode(ByVal str As String, xORnumber As Integer)                 '暗号化された文字列を渡すと復号化して返す（Ｘｏｒ結合で結合した値が必要）
Dim bytArray() As Byte
Dim valsplited As Variant
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            valsplited = Split(str, ",")
            ReDim bytArray(UBound(valsplited))
            For i = 0 To UBound(valsplited)
                bytArray(i) = CByte(valsplited(i))
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
            Next i
            For i = 0 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            OutCode = StrConv(bytArray, vbUnicode)
End Function

Public Function FileCodeIn(FileNum As String, xORnumber As Integer)              '与えたファイルの暗号化をする　※　ファイル名に拡張子がなく、また、ファイル名のみのもの、＋が付いていないもの（Ｘｏｒ結合で結合する値が必要）
Dim Dat() As Byte                'ファイルのデータ
Dim i As Long, Lf As Long      'カウンタとファイルサイズ
Dim intFilenumber As Integer                'ファイルナンバー
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

        strexist = Dir(App.Path & "\" & FileNum)       '暗号化されていないファイルが存在する場合
        If strexist = FileNum Then
            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            '暗号化するファイルのデータを読み込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '暗号化処理
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
               
            '暗号化したデータをファイルに書き込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "＋" For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
            
            Kill App.Path & "\" & FileNum        '元のファイルを削除しておく
        End If
                    
End Function

Public Function FileCodeOut(FileNum As String, xORnumber As Integer)              '与えたファイルの復号化をする　※ファイル名に拡張子がなく、また、ファイル名のみ（Ｘｏｒ結合で結合した値が必要）
Dim Dat() As Byte                'ファイルのデータ
Dim i As Long, Lf As Long      'カウンタとファイルサイズ
Dim intFilenumber As Integer                'ファイルナンバー
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer
       
       strexist = Dir(App.Path & "\" & FileNum & "＋")        'ファイルが存在する場合
       If strexist = FileNum & "＋" Then            'ファイル名に暗号化されている事を示す＋が付いているものが存在する場合
            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            '復号化するファイルのデータを読み込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "＋" For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '復号化処理
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            
            '復号化したデータをファイルに書き込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
                       
            Kill App.Path & "\" & FileNum & "＋"                  '元のファイルを削除しておく
       End If
End Function




[module1.bas]




Public Type Preserved
    strName As String * 100                 'ユーザー名保持用
    strAddres As String * 100              'クライアントのアドレス保持用
    intFileAddres As Integer              'サーバが保存しているクライアントデータファイルのどこに自分のデータが保持されているか
    strCulculated As String * 100           'クライアントがどの値まで計算したか保持
    lngNowCulculate As Long                'クライアントが前回計算した値
    lngWorkCounts As Long                  'クライアントがいくつのワーク数をこなしたか
    strMassageFrom As String * 1000         'クライアントから送信されたメッセージ
    intNowRange As Integer                '現在どこの範囲まで探索が完了したか（1番目のレコードと一緒に記録）
    intDistributeCount As Integer         '最後に割り振った値は、何回割り振ったか
    strDiscoveredPrime As String * 1000    '現在までに発見した素数
    intMemorySize As Integer              'クライアントの物理メモリ総量
    intCpuClock As Single                 'クライアントのＣＰＵのクロック数
    strCpuType As String * 100              'クライアントのＣＰＵの種類
    intPrize As Integer                    '接続を受けた時点での順位
    lngNowCulculateArray As Long            'クライアントが計算している指数を保持しているPreparePrimeの１次元目の添え字
    lngClientWorkingTime As Long            'クライアントの総起動時間を秒単位で保持
End Type

Public Const xORnumber = 173         'データを暗号化する時にxor結合で結合する値

Public Type Confirmed
    lngPrimeNumber As Long                          '指数が保持されているprepareprimeの一次元目の添え字
    intTrueOrFalse As Integer                       '素数であるかないか　1=true 2=false
End Type

Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long    ' lstrlen()関数　文字列のバイト数を得るＡＰＩの宣言
Declare Sub MoveMemory Lib "kernel32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)      ' メモリ操作関数　文字列をバイト配列に代入する
