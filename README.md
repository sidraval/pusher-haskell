#![Haskell](https://raw.githubusercontent.com/sidraval/pusher-haskell/master/assets/haskell.png)![Pusher](https://raw.githubusercontent.com/sidraval/pusher-haskell/master/assets/pusher.png)
A [Pusher](http://www.pusher.com) server client written in Haskell.

Currently, the package allows:

* Triggering events on single or multiple channels
* Fetching basic information about a given channel

Up next:

* Fetching a list of channels with active subscriptions
* Fetching a list of users present for a particular channel

## Usage
If you go to https://app.pusher.com/apps/YOUR_APP_ID/api_access and run the
following code after filling in your `app-id`, `app-key`, and `app-secret`, you
should see an alert popup on screen.

```haskell
> import Pusher

> let pusher = Pusher "app-id" "app-key" "app-secret"
> let channel = "test_channel"
> let event = Event "my_event" "{\"message\":\"hello world\"}"

> triggerEvent pusher channel event
"{}"

> getChannelInfo pusher channel
"{\"occupied\":true}"
```

You can also trigger events across multiple channels:
```haskell
> ...
> let channels = ["first_channel", "second_channel"]
> ...

> triggerEvent pusher channels event
"{}"
```

## Issues & Todo
* Use Aeson to decode responses into data structures on our end
