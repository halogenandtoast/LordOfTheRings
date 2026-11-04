module LordOfTheRings.Treachery.Cards.WeHatesThem (weHatesThem) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (smeagol)
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WeHatesThem = WeHatesThem TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

weHatesThem :: TreacheryCard WeHatesThem
weHatesThem = treachery WeHatesThem Cards.weHatesThem

gollum :: EnemyMatcher
gollum = EnemyWithCardCode (toCardCode Enemies.gollumTheLandOfShadow)

{- | Surge.
When Revealed: Either flip Sméagol to Gollum, or Gollum makes an immediate attack. The first
player may spend 1 Fellowship resource and exhaust The One Ring to cancel this effect.
The first player chooses; Sméagol and Gollum being one card, only one of them can be done.
Shadow: Attacking enemy gets +1 attack (+3 attack instead if the attacking enemy is Gollum).
-}
instance RunMessage WeHatesThem where
  runMessage msg t@(WeHatesThem attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      resources <- getResourcesOfSphere lead Fellowship
      ringExhausted <- theOneRingExhausted
      ring <- getSagaOneRing
      case ring of
        Just r | resources > 0, not ringExhausted ->
          chooseOneM lead do
            questionLabeled "weHatesThem.cancel"
            causedBy attrs
            labeled "weHatesThem.cancelEffect" do
              spendResources lead Fellowship 1
              exhaust (toTarget r)
              push $ CheckWindows [Window After $ WhenRevealedCanceled (toTarget attrs)]
            labeled "weHatesThem.resolve" $ doStep 1 msg
        _ -> doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      lead <- getLead
      smeagols <- select smeagol
      gollums <- traverse getEnemy =<< select gollum
      chooseOrRunOneM lead do
        questionLabeled "weHatesThem.choose"
        causedBy attrs
        unless (null smeagols) $ labeled "weHatesThem.flip" $ traverse_ flipOver smeagols
        for_ gollums \g -> labeled "weHatesThem.attack" do
          let defender = case g.placement of
                EngagedWith p -> p
                _ -> lead
          immediateAttack defender g.id
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      isGollum <- selectAny (EnemyWithId eid <> gollum)
      enemyGetsAttack attrs eid (if isGollum then 3 else 1)
      pure t
    _ -> WeHatesThem <$> runMessage msg attrs
