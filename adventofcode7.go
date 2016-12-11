package main

import "strings"
import "fmt"
import "bytes"

func include_abba(channel chan bool, str string) {
  for i := 0; i < (len(str) - 3); i = i + 1 {
    if str[i] != str[i+1] && str[i] == str[i+3] && str[i+1] == str[i+2] {
      channel <- true
    }
  }
  channel <- false
}

func support_tls(resp chan bool, supernets []string, hypernets []string) {
  response := false
  hypernet_support := make(chan bool)
  for _,hypernet := range hypernets {
    go include_abba(hypernet_support, hypernet)
  }

  not_supported := false
  for range hypernets {
    not_supported = (not_supported || <-hypernet_support)
  }

  if !not_supported {
    supernet_support := make(chan bool)
    for _,supernet := range supernets {
      go include_abba(supernet_support, supernet)
    }

    supported := false
    for range supernets {
      supported = (supported || <-supernet_support)
    }
    if supported {
      response = true
    }
  }

  resp <- response
}

func reverse_abas(nets []string) []string {
  var babs []string
  for _,net := range nets {
    for i := 0; i < (len(net) - 2); i = i + 1 {
      if net[i] != net[i+1] && net[i] == net[i+2] {
        var bab bytes.Buffer
        bab.WriteString(net[i+1:i+2])
        bab.WriteString(net[i:i+1])
        bab.WriteString(net[i+1:i+2])
        babs = append(babs, bab.String())
      }
    }
  }
  return babs
}

func support_ssl(resp chan bool, supernets []string, hypernets []string) {
  babs := reverse_abas(supernets)
  for _,hypernet := range hypernets {
    for _,bab := range babs {
      if strings.Contains(hypernet, bab) {
        resp <- true
      }
    }
  }
  resp <- false
}

func main() {
  var err error = nil
  var address string
  var supports_tls int = 0
  var supports_ssl int = 0

  n, err := fmt.Scanln(&address)
  for err == nil && n > 0{

    hypernet_begin := strings.Index (address, "[")
    hypernet_end := -1
    var supernets []string;
    var hypernets []string;
    for hypernet_begin != -1 {
      supernets = append(supernets, address[hypernet_end + 1:hypernet_begin])
      hypernet_end = strings.Index(address[hypernet_begin:len(address)], "]") +
                     hypernet_begin
      hypernet := address[hypernet_begin + 1:hypernet_end]
      hypernets = append(hypernets, hypernet)

      hypernet_begin = strings.Index(address[hypernet_end:len(address)], "[")
      if hypernet_begin != -1 {
        hypernet_begin = hypernet_begin + hypernet_end
      }
    }
    supernets = append(supernets, address[hypernet_end + 1:len(address)])

    tls_support := make(chan bool)
    go support_tls(tls_support, supernets, hypernets)
    ssl_support := make(chan bool)
    go support_ssl(ssl_support, supernets, hypernets)

    if <-tls_support {
      supports_tls = supports_tls + 1
    }
    if <-ssl_support {
      supports_ssl = supports_ssl + 1
    }

    n, err = fmt.Scanln(&address)
  }

  fmt.Println("Number of addresses supporting TLS: ", supports_tls)
  fmt.Println("Number of addresses supporting SSL: ", supports_ssl)
}
