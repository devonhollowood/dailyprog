extern crate hyper;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 3 {
        println!("Usage: bitcoin_price <exchange> <currency>");
        std::process::exit(2);
    }
    let ref exchange = args[1];
    let ref currency = args[2];
    if let Err(errmsg) = validate(&exchange[..], &currency[..]) {
        println!("Error: {}", errmsg);
        std::process::exit(2);
    }
    match get_price(&exchange[..], &currency[..]) {
        Ok(price) => println!("{}", price),
        Err(errmsg) => {
            println!("Error: {}", errmsg);
            std::process::exit(1)
        }
    }
}

fn validate(exchange: &str, currency: &str) -> Result<(), String> {
    const VALID_EXCHANGES: [&'static str; 13] =
        ["bitfinex", "bitstamp", "btce", "itbit", "anxhk", "hitbtc", "kraken", "bitkonan", "bitbay",
         "rock", "cbx", "cotr", "vcx"];
    const VALID_CURRENCIES: [&'static str; 39] =
        ["KRW", "NMC", "IDR", "RON", "ARS", "AUD", "BGN", "BRL", "BTC", "CAD", "CHF", "CLP", "CNY",
         "CZK", "DKK", "EUR", "GAU", "GBP", "HKD", "HUF", "ILS", "INR", "JPY", "LTC", "MXN", "NOK",
         "NZD", "PEN", "PLN", "RUB", "SAR", "SEK", "SGD", "SLL", "THB", "UAH", "USD", "XRP", "ZAR"];
    if !VALID_EXCHANGES[..].contains(&exchange) {
        Err(format!("Invalid exchange: {}. Must be one of {}.", exchange,
                    VALID_EXCHANGES.connect(", ")))
    }
    else if !VALID_CURRENCIES[..].contains(&currency) {
        Err(format!("Invalid currency: {}. Must be one of {}.", currency,
                    VALID_CURRENCIES.connect(", ")))
    }
    else {
        Ok(())
    }
}

fn get_price(exchange: &str, currency: &str) -> Result<f32, String> {
    use hyper::Client;
    use hyper::header::Connection;
    use std::io::Read;
    let client = Client::new();
    let url = format!("http://api.bitcoincharts.com/v1/trades.csv?symbol={}{}", exchange, currency);
    let mut response = match client.get(&url[..]).header(Connection::close()).send() {
        Ok(resp) => resp,
        Err(errmsg) => return Err(format!("{}", errmsg)),
    };
    let mut response_body = String::new();
    response.read_to_string(&mut response_body).unwrap();
    let most_recent_exchange = match response_body.split_whitespace().next() {
        Some(exchange) => exchange,
        None           => return Err(format!("No data found for {}, {}", exchange, currency)),
    };
    let price = most_recent_exchange.split(",").nth(1).unwrap().parse::<f32>().unwrap();
    Ok(price)
}
