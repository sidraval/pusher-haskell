#![Haskell](https://raw.githubusercontent.com/sidraval/pusher-haskell/master/assets/haskell.png)![Pusher](https://raw.githubusercontent.com/sidraval/pusher-haskell/master/assets/pusher.png)
A [Pusher](http://www.pusher.com) server client written in Haskell.

## Usage
An example:

```haskell
import Pusher

let pusher = Pusher "app-id", "app-key", "app-secret"
let channel = "my-pusher-channel"
let event = Event "event-name" "event-data"

triggerEvent pusher channel event
```
