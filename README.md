# rgooglemaps


### *Attention!*

Google has [recently changed its API
requirements](https://developers.google.com/maps/documentation/geocoding/usage-and-billing),
and users are now required to register with Google. From a
user’s perspective, there are essentially three ramifications of this:

1.  Users must register with Google. You can do this at
    <a href="https://cloud.google.com/maps-platform/" class="uri">https://cloud.google.com/maps-platform/</a>.
    While it will require a valid credit card (sorry!), there seems to
    be a fair bit of free use before you incur charges, and even then
    the charges are modest for light use.

2.  Users must enable the APIs they intend to use. What may appearas one overarching “Google Maps” product, Google in
    fact has several services that it provides as geo-related solutions.
    For example, the [Maps Static
    API](https://developers.google.com/maps/documentation/maps-static/intro)
    provides map images, while the [Geocoding
    API](https://developers.google.com/maps/documentation/geocoding/intro)
    provides geocoding and reverse geocoding services. Apart from the
    relevant Terms of Service, generally rgooglemaps users don’t need to
    think about the different services. *However*, you do need to enable the APIs before you use
    them. You’ll only need to do that once, and then they’ll be ready
    for you to use. Enabling the APIs just means clicking a few radio
    buttons on the Google Maps Platform web interface listed above, so
    it’s easy.


Your API key is *private* and unique to you, so be careful not to share
it online, for example in a GitHub issue or saving it in a shared R
script file. If you share it inadvertantly, just get on Google’s website
and regenerate your key - this will retire the old one. 


Installation
------------

-   From CRAN: `install.packages("rgooglemaps")`

-   From Github:

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("markusloecher/rgooglemaps")
```


