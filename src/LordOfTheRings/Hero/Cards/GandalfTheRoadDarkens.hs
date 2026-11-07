module LordOfTheRings.Hero.Cards.GandalfTheRoadDarkens (gandalfTheRoadDarkens) where

import LordOfTheRings.ActiveCost (ActiveCost (activeCostCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameActiveCosts, gameEffects)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))

newtype GandalfTheRoadDarkens = GandalfTheRoadDarkens HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gandalfTheRoadDarkens :: HeroCard GandalfTheRoadDarkens
gandalfTheRoadDarkens = hero GandalfTheRoadDarkens Cards.gandalfTheRoadDarkens

{- | Play with the top card of your deck faceup. Once per phase, you may play the top card
of your deck as if it was in your hand. When playing a card this way, Gandalf is considered
to have the printed [leadership], [lore], [tactics], and [spirit] icons.

The card played from the top of the deck marks Gandalf 'PlayedTopOfDeckThisPhase' (see
'MayPlayTopOfDeck'), which ends the play for the phase. The card has left the deck by the
time it is paid for, so the icons follow that card while its cost is active.
-}
instance HasModifiersFor GandalfTheRoadDarkens where
  getModifiersFor (GandalfTheRoadDarkens a) = do
    modified a (PlayerTarget a.owner) [TopOfDeckFaceup]
    used <- getsGame \g -> [cid | e <- gameEffects g, effectTarget e == HeroTarget a.id, PlayedTopOfDeckThisPhase cid <- effectModifiers e]
    paying <- getsGame (map ((.id) . activeCostCard) . toList . gameActiveCosts)
    cards <-
      if null used
        then do
          modified a (PlayerTarget a.owner) [MayPlayTopOfDeck]
          map (.id) . take 1 . (.deck) <$> getPlayer a.owner
        else pure $ filter (`elem` paying) used
    modifySelf a [PrintedResourceIconForCard cid s | cid <- cards, s <- [Leadership, Lore, Tactics, Spirit]]
