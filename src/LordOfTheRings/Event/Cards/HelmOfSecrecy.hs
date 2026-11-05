module LordOfTheRings.Event.Cards.HelmOfSecrecy (helmOfSecrecy) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card (PlayerCard), cdCardType, cdSphere, cdThreatCost, genPlayerCard, toCardCode, toCardDef)
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
-- collection is every hero card the game has, those the unique rule lets into play as the
-- chosen hero leaves it, but for its Fellowship and Baggins heroes.
helmOfSecrecy :: EventCard HelmOfSecrecy
helmOfSecrecy = event HelmOfSecrecy Cards.helmOfSecrecy

instance RunMessage HelmOfSecrecy where
  runMessage msg e@(HelmOfSecrecy attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      -- a scenario's objective heroes, as Kahliel, are no hero cards of a collection
      let inCollection code = cdCardType (toCardDef code) == HeroType && cdSphere (toCardDef code) `notElem` [Just Fellowship, Just Baggins]
      let threatOf = fromMaybe 0 . cdThreatCost . toCardDef
      heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "helmOfSecrecy.chooseHero"
        causedBy attrs
        targets heroes \h -> do
          for_ (Map.toList $ heroTokens $ toAttrs h) \(token, n) -> removeTokens attrs h token n
          -- a different card: another version of the chosen hero may be, as it leaves
          -- play in the swap, as Caleb Grace's Thorongil ruling assumes (ALeP FAQ 3.374)
          let candidates = [code | code <- Map.keys allHeroes, inCollection code, code /= toCardCode h, threatOf code <= h.threat]
          cards <- filterM (passesUniqueReplacing h.id . PlayerCard) =<< traverse (\code -> genPlayerCard (toCardDef code) (Just pid)) candidates
          unless (null cards)
            $ push
              $ Ask pid
              $ QuestionLabel (ikey "label.helmOfSecrecy.chooseSwap") (Just $ toTarget attrs) []
              $ SearchCards (map PlayerCard cards)
              $ ChooseOne [targetLabel (CardIdTarget c.id) [SwapHero h.id c] | c <- cards]
      pure e
    _ -> HelmOfSecrecy <$> runMessage msg attrs
