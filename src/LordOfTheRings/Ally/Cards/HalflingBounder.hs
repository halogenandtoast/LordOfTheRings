module LordOfTheRings.Ally.Cards.HalflingBounder (halflingBounder) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype HalflingBounder = HalflingBounder AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

halflingBounder :: AllyCard HalflingBounder
halflingBounder = ally HalflingBounder Cards.halflingBounder

-- | While there is a side quest in the victory display, Halfling Bounder gains:
-- "Response: Discard Halfling Bounder to cancel the 'when revealed' effects of an encounter
-- card that was just revealed from the encounter deck."
instance HasAbilities HalflingBounder where
  getAbilities (HalflingBounder a) =
    [ restricted a 1 (CardInVictoryDisplay sideQuestCard <> EncounterEffectsCancelable)
        $ response
        $ CardRevealedWindow When
    ]

instance RunMessage HalflingBounder where
  runMessage msg s@(HalflingBounder a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      discard a
      for_ [t | Window _ (Revealed t) <- windows] cancelWhenRevealed
      pure s
    _ -> HalflingBounder <$> runMessage msg a
