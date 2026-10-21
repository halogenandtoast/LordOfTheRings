module LordOfTheRings.Event.Cards.HelmOfSecrecy (helmOfSecrecy) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card (PlayerCard), cdThreatCost, genPlayerCard, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Hero (allHeroes)

newtype HelmOfSecrecy = HelmOfSecrecy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a hero you control and discard all tokens from it. Select a different
-- non-fellowship, non-baggins hero from your collection with a threat cost less than or
-- equal to the chosen hero. Swap those heroes, moving all eligible attachments to the hero
-- from your collection.
--
-- ponytail: your collection is every hero the engine knows that the unique rule lets into
-- play; there are no fellowship or baggins heroes yet to leave out.
helmOfSecrecy :: EventCard HelmOfSecrecy
helmOfSecrecy = event HelmOfSecrecy Cards.helmOfSecrecy

instance RunMessage HelmOfSecrecy where
  runMessage msg e@(HelmOfSecrecy attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      collection <- flip filterM (Map.keys allHeroes) \code -> passesUnique . PlayerCard =<< genPlayerCard (toCardDef code) (Just pid)
      let threatOf = fromMaybe 0 . cdThreatCost . toCardDef
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "helmOfSecrecy.chooseHero"
        causedBy attrs
        for_ heroes \h -> do
          let swaps = [code | code <- collection, threatOf code <= h.threat]
          unless (null swaps) $ targeting h do
            removeTokens attrs h Damage h.damage
            removeTokens attrs h Resource h.resources
            cards <- traverse (\code -> genPlayerCard (toCardDef code) (Just pid)) swaps
            push
              $ Ask pid
              $ QuestionLabel (ikey "label.helmOfSecrecy.chooseSwap") (Just $ toTarget attrs) []
              $ SearchCards (map PlayerCard cards)
              $ ChooseOne [targetLabel (CardIdTarget c.id) [SwapHero h.id c] | c <- cards]
      pure e
    _ -> HelmOfSecrecy <$> runMessage msg attrs
