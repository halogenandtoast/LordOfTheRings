module LordOfTheRings.Treachery.Cards.CatchingFire (catchingFire) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Trait (Trait (Pier))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CatchingFire = CatchingFire TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

catchingFire :: TreacheryCard CatchingFire
catchingFire = treachery CatchingFire Cards.catchingFire

instance RunMessage CatchingFire where
  runMessage msg t@(CatchingFire a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      piers <- getsGame $ filter (cardMatches $ CardWithTrait Pier) . (.victoryDisplay) . toAttrs . (.scenario)
      lead <- getLead
      if null piers
        then phaseModifier a a (GainKeyword Surge)
        else chooseOrRunOneM lead do
          questionLabeled "lakeTown.returnPier"
          causedBy a
          targets piers $ shuffleVictoryDisplayIntoEncounterDeck . cardIs . toCardDef
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \target -> attackModifier (CardIdSource cid) target (DefenseModifier (-2))
      pure t
    _ -> CatchingFire <$> runMessage msg a
