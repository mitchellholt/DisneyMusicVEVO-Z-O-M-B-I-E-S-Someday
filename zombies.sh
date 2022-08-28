out=$(./src/Main $1)

if [[ "$out" == "sotrue" ]]; then 
    open sotrue.png
else
    open notsotrue.png
fi