module LordOfTheRings.Event.Cards.HelmOfSecrecy (helmOfSecrecy) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card (PlayerCard), cdCardType, cdSphere, cdThreatCost, genPlayerCard, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Hero (allHeroes)
import LordOfTheRings.Hero.Types (HeroAttrs (heroTokens))
import LordOfTheRings.Sphere (Sphere (..))

newtype HelmOfSecrecy = HelmOfSecrecy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a hero you control and discard all tokens from it. Select a different
-- non-fellowship, non-baggins hero from your collection with a threat cost less than or
-- equal to the chosen hero. Swap those heroes, moving all eligible attachments and damage
-- tokens to the hero from your collection. (Errata: its damage was discarded with its tokens,
-- so the hero swapped in comes in fully healed (Caleb Grace, ALeP FAQ 3.391), ready or
-- exhausted as the one it replaces (ALeP FAQ 3.367).)
--
-- Any hero you control may be chosen, its tokens discarded; the swap happens only if your
-- collection holds a hero it can be, as none can for a hero of threat cost 0. Your
-- collection is every hero card the game has, those the unique rule lets into play, but for
-- its Fellowship and Baggins heroes.
helmOfSecrecy :: EventCard HelmOfSecrecy
helmOfSecrecy = event HelmOfSecrecy Cards.helmOfSecrecy

instance RunMessage HelmOfSecrecy where
  runMessage msg e@(HelmOfSecrecy attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      -- a scenario's objective heroes, as Kahliel, are no hero cards of a collection
      let inCollection code = cdCardType (toCardDef code) == HeroType && cdSphere (toCardDef code) `notElem` [Just Fellowship, Just Baggins]
      collection <- flip filterM (filter inCollection $ Map.keys allHeroes) \code ->
        passesUnique . PlayerCard =<< genPlayerCard (toCardDef code) (Just pid)
      let threatOf = fromMaybe 0 . cdThreatCost . toCardDef
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "helmOfSecrecy.chooseHero"
        causedBy attrs
        targets heroes \h -> do
          for_ (Map.toList $ heroTokens $ toAttrs h) \(token, n) -> removeTokens attrs h token n
          let swaps = [code | code <- collection, threatOf code <= h.threat]
          cards <- traverse (\code -> genPlayerCard (toCardDef code) (Just pid)) swaps
          unless (null cards)
            $ push
              $ Ask pid
              $ QuestionLabel (ikey "label.helmOfSecrecy.chooseSwap") (Just $ toTarget attrs) []
              $ SearchCards (map PlayerCard cards)
              $ ChooseOne [targetLabel (CardIdTarget c.id) [SwapHero h.id c] | c <- cards]
      pure e
    _ -> HelmOfSecrecy <$> runMessage msg attrs
