module LordOfTheRings.Treachery.Cards.WickedMaster (wickedMaster) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WickedMaster = WickedMaster TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wickedMaster :: TreacheryCard WickedMaster
wickedMaster = treachery WickedMaster Cards.wickedMaster

gollum :: EnemyMatcher
gollum = EnemyWithCardCode (toCardCode Enemies.gollumMountDoom)

{- | When Revealed: Gollum makes an immediate attack against the first player. If Gollum is
not in play, search the encounter deck and discard pile for Gollum and put him into play
engaged with the first player. With no victory points, Gollum is never in the victory
display the search also looks through.
Shadow: If attacking enemy is Gollum, he makes an additional attack after this one.
-}
instance RunMessage WickedMaster where
  runMessage msg t@(WickedMaster attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      inPlay <- select gollum
      case inPlay of
        eid : _ -> immediateAttack lead eid
        [] -> searchEncounterCardsAndVictoryDisplay (cardIs Enemies.gollumMountDoom) (putEnemyIntoPlayEngaged lead)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      isGollum <- selectAny (gollum <> EnemyWithId eid)
      when isGollum $ additionalAttack pid eid
      pure t
    _ -> WickedMaster <$> runMessage msg attrs
