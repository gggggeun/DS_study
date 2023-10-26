## while ~ (not) in ~

```python
selected = []
while selected not in ['가위', '바위', '보']:
	selected = input('가위, 바위, 보 중에 선택하세요>')
    
print('선택된 값은: ', selected)
```
![image](https://user-images.githubusercontent.com/74692845/137425253-3218b36e-f584-4e0a-8ed3-708b28d80406.png)


## if문으로 변경할 경우?
```
selected = []
if selected not in ['가위', '바위', '보']:
	selected = input('가위, 바위, 보 중에 선택하세요>')
    
print('선택된 값은: ', selected)
```
![image](https://user-images.githubusercontent.com/74692845/137425269-d99f677c-4186-4f06-a3f2-0bb35c2a670a.png)


## while i < len(list)

```
patterns = ['가위', '보', '보']
length = len(patterns)
i = 0
while i < length:
  print(patterns[i])
  i = i + 1
```
![image](https://user-images.githubusercontent.com/74692845/137425303-8ec2b048-0e4b-4541-83d7-699198b5b58a.png)


## if문 range

```
patterns = ['가위', '보', '보']

for i in range(len(patterns)):
  print(patterns[i])
```
![image](https://user-images.githubusercontent.com/74692845/137425330-dddd03a6-2aa4-4bd9-a4e0-b5ee491fb582.png)


