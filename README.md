#![Haskell](https://raw.githubusercontent.com/sidraval/pusher-haskell/master/assets/haskell.png)![Pusher](https://raw.githubusercontent.com/sidraval/pusher-haskell/master/assets/pusher.png)
A [Pusher](http://www.pusher.com) server client written in Haskell.

Currently, the package only allows triggering events on a single channel. Next
up:

* Fetching a list of channels with active subscriptions
* Fetching information for a particular channel
* Fetching a list of users present for a particular channel

## Usage
If you go to https://app.pusher.com/apps/YOUR_APP_ID/api_access and run the
following code after filling in your `app-id`, `app-key`, and `app-secret`, you
should see an alert popup on screen.

```haskell
> import Pusher

> let pusher = Pusher "app-id", "app-key", "app-secret"
> let channel = "test_channel"
> let event = Event "my_event" "\"{\\\"message\\\":\\\"hello world\\\"}\""

> triggerEvent pusher channel event
"{}"
```

You can also trigger events across multiple channels:
```haskell
> ...
> let channels = ["first_channel", "second_channel"]
> ...

> triggerEvent pusher channels event
["{}", "{}"]
```

## Issues
* The user should not have to provided encoded JSON as in the above example
