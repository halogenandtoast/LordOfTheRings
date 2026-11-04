module LordOfTheRings.Hero.Cards.GandalfTheRoadDarkens (gandalfTheRoadDarkens) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
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
'MayPlayTopOfDeck'), which ends the play for the phase.
-}
instance HasModifiersFor GandalfTheRoadDarkens where
  getModifiersFor (GandalfTheRoadDarkens a) = do
    modified a (PlayerTarget a.owner) [TopOfDeckFaceup]
    used <- getsGame \g -> or [PlayedTopOfDeckThisPhase `elem` effectModifiers e | e <- gameEffects g, effectTarget e == HeroTarget a.id]
    unless used do
      modified a (PlayerTarget a.owner) [MayPlayTopOfDeck]
      top <- take 1 . (.deck) <$> getPlayer a.owner
      for_ top \c -> modifySelf a [PrintedResourceIconForCard c.id s | s <- [Leadership, Lore, Tactics, Spirit]]
