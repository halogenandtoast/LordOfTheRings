module LordOfTheRings.Enemy.Cards.WerewolfOfAngmar (werewolfOfAngmar) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype WerewolfOfAngmar = WerewolfOfAngmar EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

werewolfOfAngmar :: EnemyCard WerewolfOfAngmar
werewolfOfAngmar = enemy WerewolfOfAngmar Cards.werewolfOfAngmar

-- | Forced: After an event card is played, deal 1 shadow card to Werewolf of Angmar.
instance HasAbilities WerewolfOfAngmar where
  getAbilities (WerewolfOfAngmar a) = [mkAbility a 1 $ forced $ CardPlayedWindow After (CardWithType EventType)]

-- | Forced: If there are 3 shadow cards dealt to Werewolf of Angmar and it is in the
-- staging area, it engages the first player and makes an immediate attack (do not deal it
-- another shadow card for this attack).
instance RunMessage WerewolfOfAngmar where
  runMessage msg e@(WerewolfOfAngmar attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      dealShadowCard attrs.id
      pure e
    AttachShadowCard eid _
      | eid == attrs.id
      , attrs.placement == StagingArea
      , length attrs.shadowCards + 1 >= 3 -> do
          lead <- getLead
          engageEnemy lead attrs.id
          push $ EnemyAttack lead attrs.id
          phase <- getsGame (.phase)
          when (phase /= CombatPhase) $ push $ DiscardShadowCards attrs.id
          WerewolfOfAngmar <$> runMessage msg attrs
    _ -> WerewolfOfAngmar <$> runMessage msg attrs
